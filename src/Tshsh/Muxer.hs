{-# LANGUAGE ViewPatterns #-}

module Tshsh.Muxer
  ( module Tshsh.Muxer.Types,
    module Tshsh.Muxer.Body,
    module Tshsh.Commands,
    module Tshsh.Puppet,
    newPuppet,
    openMuxLog,
    setupSignalHandlers,
    forkReadUserInput,
    muxLoop,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe (tryIO)
import Control.Lens
import Control.Monad
import qualified Data.Set as Set
import Data.Strict.Tuple.Extended
import Data.String.Conversions
import Foreign hiding (void)
import Protolude hiding (tryIO)
import System.IO (hGetBufSome, hWaitForInput)
import System.Posix
import System.Posix.Signals.Exts
import System.Process
import Tshsh.Commands
import qualified Tshsh.Constants as Const
import Tshsh.Data.BufferSlice (BufferSlice (..))
import qualified Tshsh.Data.BufferSlice as BufferSlice
import Tshsh.Lang.Coroutine.CPS
import Tshsh.Muxer.Body
import Tshsh.Muxer.Log
import Tshsh.Muxer.ShellOutputParser
import qualified Tshsh.Muxer.TuiModeMatcher as TuiMatcher
import Tshsh.Muxer.Types
import Tshsh.Puppet
import Tshsh.Tty

readLoopStep :: ReadLoopSt -> IO (Maybe (BufferSlice, ReadLoopSt))
readLoopStep st0 =
  if _rl_capacity st0 >= Const.minBufSize
    then readSlice st0
    else do
      buff <- mallocForeignPtrBytes Const.bufSize
      readSlice
        ( st0
            { _rl_capacity = Const.bufSize,
              _rl_buff = buff,
              _rl_dataPtr = buff
            }
        )
  where
    readSlice st@ReadLoopSt {..} =
      withForeignPtr _rl_dataPtr (\ptr -> hGetBufSome _rl_fileHandle ptr _rl_capacity) >>= \case
        n | n > 0 -> do
          let res = BufferSlice _rl_buff _rl_dataPtr n
          pure $
            Just
              ( res,
                st
                  { _rl_capacity = _rl_capacity - n,
                    _rl_dataPtr = _rl_dataPtr `plusForeignPtr` n
                  }
              )
        _ -> pure Nothing

readLoopInit :: Handle -> IO ReadLoopSt
readLoopInit h = do
  buff <- mallocForeignPtrBytes Const.bufSize
  pure (ReadLoopSt Const.bufSize buff buff h)

readLoop :: Text -> Handle -> (BufferSlice -> IO ()) -> IO ()
readLoop name fromH act = do
  let loop st0 = do
        readLoopStep st0 >>= \case
          Just (res, st) -> do
            act res
            loop st
          Nothing ->
            pure ()
  res <- tryIO (loop =<< readLoopInit fromH)
  case res of
    Left err -> hPutStrLn stderr (name <> " " <> show err)
    Right _ -> pure ()

newPuppet ::
  PuppetIdx ->
  PuppetCfg ->
  TVar (Set PuppetIdx) ->
  IO Puppet
newPuppet idx cfg@PuppetCfg {..} dataAvail = do
  let outParser = toEv (raceMatchersP `pipe` accumCmdOutP `pipe` stripCmdOutP)

  let outParserSt =
        OutputParserSt
          { _op_promptMatcher = _pc_promptMatcher,
            _op_tuiModeMatcher = TuiMatcher.tuiModeMatcher,
            _op_mode = PuppetModeRepl,
            _op_currCmdOut = RawCmdResult BufferSlice.listEmpty
          }

  let startProcess = do
        (master, slave) <- openPseudoTerminal
        masterH <- fdToHandle master
        slaveH <- fdToHandle slave
        pts <- getSlaveTerminalName master

        -- _ <- system ("stty -F " <> pts <> " $(stty --save)")
        syncTtySize pts

        (_, _, _, p) <-
          createProcess
            -- To implement jobs control a process needs
            -- 1. to become a session leader
            -- 2. to acquire a controlling terminal so that it's children inherit the same terminal
            (proc "acquire_tty_wrapper" (fmap cs (_pc_cmd : _pc_cmdArgs)))
              { std_in = UseHandle slaveH,
                std_out = UseHandle slaveH,
                std_err = UseHandle slaveH
                -- new_session = True,
                -- create_group = False,
                -- this one is not implemented in rts that why we wrote an acquire_tty_wrapper
                -- aquire_tty = True
              }
        -- TODO: handle process startup failures
        (Just pid) <- getPid p

        readSlice <- readLoopInit masterH

        watchFileInput masterH dataAvail (Set.insert idx)

        hPutStrLn stderr ("Started: " <> (show pid :: Text))
        pure $
          PuppetState
            { _ps_idx = idx,
              _ps_cfg = cfg,
              _ps_outputParser = (outParserSt :!: outParser),
              _ps_process =
                PuppetProcess
                  { _pp_handle = p,
                    _pp_pid = pid,
                    _pp_inputH = masterH,
                    _pp_pts = pts,
                    _pp_readSliceSt = readSlice
                  }
            }
  pure
    ( Puppet
        { _pup_idx = idx,
          _pup_cfg = cfg,
          _pup_cmd = _pc_cmd,
          _pup_cmdArgs = _pc_cmdArgs,
          _pup_promptMatcher = _pc_promptMatcher,
          _pup_getCwdCmd = _pc_getCwdCmd,
          _pup_mkCdCmd = _pc_mkCdCmd,
          _pup_startProcess = startProcess,
          _pup_switchEnterHook = _pc_switchEnterHook,
          _pup_switchExitHook = _pc_switchExitHook,
          _pup_cleanPromptP = _pc_cleanPromptP
        }
    )

watchFileInput :: Handle -> TVar a -> (a -> a) -> IO ()
watchFileInput h var act = do
  void . forkIO $ do
    _ <- hWaitForInput h (-1)
    atomically $ modifyTVar var act

muxLoop_ :: TQueue MuxCmd -> TVar (Set PuppetIdx) -> MuxEnv -> MuxState -> IO ()
muxLoop_ !queue !dataAvail !env !st0 = do
  (inpMsg, readyPup) :: ([MuxCmd], Set PuppetIdx) <- waitInput

  let loop [] st = pure st
      loop (x : xs) st = handlePuppetInput x st >>= loop xs

  handleInpMsg inpMsg (Just st0)
    >>= loop (Set.toList readyPup)
    >>= \case
      Nothing -> pure ()
      Just newSt -> muxLoop_ queue dataAvail env newSt
  where
    waitInput :: IO ([MuxCmd], Set PuppetIdx)
    waitInput =
      atomically $ do
        inpMsg <- flushTQueue queue
        readyPup <- swapTVar dataAvail Set.empty
        if null inpMsg && Set.null readyPup
          then retry
          else pure (inpMsg, readyPup)

    muxBodyL e s cmd = muxLog cmd >> muxBody e s cmd

    handleInpMsg [] st = pure st
    handleInpMsg _ Nothing = pure Nothing
    handleInpMsg (cmd : rest) (Just st) = muxBodyL env st cmd >>= handleInpMsg rest

    muxReadFromPuppet :: MuxState -> PuppetIdx -> IO (Maybe BufferSlice, MuxState)
    muxReadFromPuppet st idx =
      case st ^? mst_puppets . ix idx of
        Nothing -> pure (Nothing, st)
        Just (_ps_process -> pp) -> do
          res <-
            readLoopStep (_pp_readSliceSt pp) >>= \case
              Nothing -> pure (Nothing, st)
              Just (slice, newReadSt) -> do
                let newSt =
                      st & mst_puppets . ix idx . ps_process
                        .~ (pp {_pp_readSliceSt = newReadSt})
                pure (Just slice, newSt)
          watchFileInput (_pp_inputH pp) dataAvail (Set.insert idx)
          pure res

    handlePuppetInput _ Nothing = pure Nothing
    handlePuppetInput idx (Just st) = do
      (mSlice, newSt) <- muxReadFromPuppet st idx
      case mSlice of
        Nothing -> pure (Just newSt)
        Just slice -> do
          muxBodyL env newSt (PuppetOutput idx slice)

muxLoop :: Mux -> IO ()
muxLoop (Mux q d e s) = muxLoop_ q d e s

setupSignalHandlers :: TQueue MuxCmd -> IO ()
setupSignalHandlers queue = do
  -- catch susp and child inpMsg
  _ <- installHandler windowChange (Catch (atomically $ writeTQueue queue WindowResize)) Nothing
  let suspendSig = atomically $ writeTQueue queue SwitchPuppet
  _ <- installHandler keyboardStop (Catch suspendSig) Nothing

  _ <- setStoppedChildFlag True
  let onChildSig :: SignalInfo -> IO ()
      onChildSig (SignalInfo _ _ NoSignalSpecificInfo) = pure ()
      onChildSig (SignalInfo _ _ SigChldInfo {..}) = do
        hPutStr stderr ("Sig> Child status: " <> show siginfoPid <> " -> " <> show siginfoStatus <> "\n" :: Text)
        atomically $ writeTQueue queue (ChildExited siginfoPid)
  _ <- installHandler processStatusChanged (CatchInfo onChildSig) Nothing
  pure ()

forkReadUserInput :: TQueue MuxCmd -> IO ()
forkReadUserInput queue = do
  void . forkIO $
    readLoop "[Read stdin thread]" stdin $ \str ->
      atomically . writeTQueue queue $ TermInput str
