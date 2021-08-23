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
  IO (Puppet, PuppetState)
newPuppet idx PuppetCfg {..} = do
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

        dataAvailable <- newTVarIO False
        readSlice <- readLoopInit masterH

        watchHandleInput masterH dataAvailable

        hPutStrLn stderr ("Started: " <> (show pid :: Text))
        pure $
          PuppetProcess
            { _pp_handle = p,
              _pp_pid = pid,
              _pp_inputH = masterH,
              _pp_pts = pts,
              _pp_dataAvailable = dataAvailable,
              _pp_readSliceSt = readSlice
            }

  let outParser = toEv (raceMatchersP `pipe` accumCmdOutP `pipe` stripCmdOutP)

  let puppetState =
        PuppetState
          { _ps_idx = idx,
            _ps_promptMatcher = _pc_promptMatcher,
            _ps_tuiModeMatcher = TuiMatcher.tuiModeMatcher,
            _ps_mode = PuppetModeRepl,
            _ps_currCmdOut = RawCmdResult BufferSlice.listEmpty,
            _ps_outputParser = outParser,
            _ps_process = Nothing
          }
  pure
    ( Puppet
        { _pup_idx = idx,
          _pup_cmd = _pc_cmd,
          _pup_cmdArgs = _pc_cmdArgs,
          _pup_promptMatcher = _pc_promptMatcher,
          _pup_getCwdCmd = _pc_getCwdCmd,
          _pup_mkCdCmd = _pc_mkCdCmd,
          _pup_startProcess = startProcess,
          _pup_initState = puppetState,
          _pup_switchEnterHook = _pc_switchEnterHook,
          _pup_switchExitHook = _pc_switchExitHook,
          _pup_cleanPromptP = _pc_cleanPromptP
        },
      puppetState
    )

watchHandleInput :: Handle -> TVar Bool -> IO ()
watchHandleInput h var = do
  void . forkIO $ do
    _ <- hWaitForInput h (-1)
    atomically $ writeTVar var True

muxLoop_ :: TQueue MuxCmd -> MuxEnv -> MuxState -> IO ()
muxLoop_ !queue !env !st0 = do
  (inpMsg, d1Av, d2Av) :: ([MuxCmd], Bool, Bool) <- waitInput

  handleInpMsg inpMsg (Just st0)
    >>= whenAv d1Av (handlePuppetInput Puppet1)
    >>= whenAv d2Av (handlePuppetInput Puppet2)
    >>= \case
      Nothing -> pure ()
      Just newSt -> muxLoop_ queue env newSt
  where
    waitInput :: IO ([MuxCmd], Bool, Bool)
    waitInput = do
      let (p1 :!: p2) = _mst_puppetSt st0
          md1Av = p1 ^? ps_process . _Just . pp_dataAvailable
          md2Av = p2 ^? ps_process . _Just . pp_dataAvailable

      atomically $ do
        inpMsg <- flushTQueue queue
        av1 <- maybe (pure False) readTVar md1Av
        av2 <- maybe (pure False) readTVar md2Av
        if not (null inpMsg) || av1 || av2
          then pure (inpMsg, av1, av2)
          else retry

    whenAv True act st = act st
    whenAv False _ st = pure st

    muxBodyL e s cmd = muxLog cmd >> muxBody e s cmd

    handleInpMsg [] st = pure st
    handleInpMsg _ Nothing = pure Nothing
    handleInpMsg (cmd : rest) (Just st) = muxBodyL env st cmd >>= handleInpMsg rest

    muxReadFromPuppet :: MuxState -> PuppetIdx -> IO (Maybe BufferSlice, MuxState)
    muxReadFromPuppet st idx =
      case st ^. mst_puppetSt . pupIdx idx . ps_process of
        Nothing -> pure (Nothing, st)
        Just pp -> do
          wasAv <- atomically $ swapTVar (_pp_dataAvailable pp) False
          res <-
            readLoopStep (_pp_readSliceSt pp) >>= \case
              Nothing -> pure (Nothing, st)
              Just (slice, newReadSt) -> do
                let newSt =
                      st & mst_puppetSt . pupIdx idx . ps_process
                        ?~ (pp {_pp_readSliceSt = newReadSt})
                pure (Just slice, newSt)
          when wasAv (watchHandleInput (_pp_inputH pp) (_pp_dataAvailable pp))
          pure res

    handlePuppetInput _ Nothing = pure Nothing
    handlePuppetInput idx (Just st) = do
      (mSlice, newSt) <- muxReadFromPuppet st idx
      case mSlice of
        Nothing -> pure (Just newSt)
        Just slice -> do
          muxBodyL env newSt (PuppetOutput idx slice)

muxLoop :: Mux -> IO ()
muxLoop (Mux q e s) = muxLoop_ q e s

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
