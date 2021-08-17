{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe (tryIO)
import Control.Lens
import Control.Monad
import Data.Strict.Tuple.Extended
import Data.String.Conversions
import qualified Data.Text.IO as T
import Foreign hiding (void)
import Protolude hiding (tryIO)
import ShellConfig
import System.Directory
import System.IO (BufferMode (..), hFlush, hGetBufSome, hPrint, hSetBinaryMode, hSetBuffering, hWaitForInput)
import System.IO.Unsafe
import System.Posix
import System.Posix.Signals.Exts
import System.Process
import Tshsh.Commands
import Tshsh.Data.BufferSlice (BufferSlice (..))
import qualified Tshsh.Data.BufferSlice as BufferSlice
import Tshsh.Lang.Coroutine.CPS
import Tshsh.Muxer
import Tshsh.Muxer.ShellOutputParser
import qualified Tshsh.Muxer.TuiModeMatcher as TuiMatcher
import Tshsh.Puppet
import Prelude (String)

muxLogFile :: MVar Handle
muxLogFile = unsafePerformIO newEmptyMVar
{-# NOINLINE muxLogFile #-}

muxLog :: Show a => a -> IO ()
muxLog a =
  tryReadMVar muxLogFile >>= \case
    Nothing -> pure ()
    Just h -> hPrint h a

bufSize :: Int
bufSize = 64 * 1024

minBufSize :: Int
minBufSize = 64

-- data ReadLoopSt = ReadLoopSt
--   { _rl_capacity :: Int,
--     _rl_buff :: ForeignPtr Word8,
--     _rl_dataPtr :: ForeignPtr Word8,
--     _rl_fileHandle :: Handle
--   }

readLoopStep :: ReadLoopSt -> IO (Maybe (BufferSlice, ReadLoopSt))
readLoopStep st0 =
  if _rl_capacity st0 >= minBufSize
    then readSlice st0
    else do
      buff <- mallocForeignPtrBytes bufSize
      readSlice
        ( st0
            { _rl_capacity = bufSize,
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
  buff <- mallocForeignPtrBytes bufSize
  pure (ReadLoopSt bufSize buff buff h)

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

  let puppetState =
        PuppetState
          { _ps_idx = idx,
            _ps_promptMatcher = _pc_promptMatcher,
            _ps_tuiModeMatcher = TuiMatcher.tuiModeMatcher,
            _ps_mode = PuppetModeRepl,
            _ps_currCmdOut = RawCmdResult BufferSlice.listEmpty,
            _ps_prevCmdOut = RawCmdResult BufferSlice.listEmpty,
            _ps_outputParser = raceMatchersP `Pipe` accumCmdOutP,
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

stderrToFile :: Text -> IO ()
stderrToFile fName = do
  hFlush stderr
  logFileFd <- handleToFd =<< openFile (cs fName) AppendMode
  _ <- dupTo logFileFd stdError
  closeFd logFileFd
  hSetBuffering stderr LineBuffering

openMuxLog :: String -> IO ()
openMuxLog fName = do
  muxF <- openFile fName AppendMode
  hSetBuffering muxF LineBuffering
  hSetBinaryMode muxF True
  putMVar muxLogFile muxF

saveTtyState :: IO Text
saveTtyState = do
  (_, Just sttyOut, _, _) <-
    createProcess
      (proc "stty" ["--save"])
        { std_in = Inherit,
          std_out = CreatePipe,
          std_err = CreatePipe,
          new_session = True
        }
  T.hGetContents sttyOut

restoreStdinTty :: Text -> IO ()
restoreStdinTty sttyState = do
  _ <- system ("stty " <> cs sttyState)
  pure ()

ensureCmdExits :: Text -> IO ()
ensureCmdExits cmd = do
  findExecutable (cs cmd) >>= \case
    Nothing -> do
      putStrLn ("Error: can't find executable " <> show cmd :: Text)
      exitFailure
    Just _ -> pure ()

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

setSignalHandlers :: TQueue MuxCmd -> IO ()
setSignalHandlers queue = do
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

configureStdinTty :: IO Text
configureStdinTty = do
  origTtyState <- saveTtyState

  -- disable all the signale except for susp
  let sttySignals = ["intr", "eof", "quit", "erase", "kill", "eol", "eol2", "swtch", "start", "stop", "rprnt", "werase", "lnext", "discard"]
  _ <- callCommand ("stty raw -echo isig susp ^Z " <> mconcat [x <> " '' " | x <- sttySignals])
  pure origTtyState

forkReadUserInput :: TQueue MuxCmd -> IO ()
forkReadUserInput queue = do
  void . forkIO $
    readLoop "[Read stdin thread]" stdin $ \str ->
      atomically . writeTQueue queue $ TermInput str

main :: IO ()
main = do
  args <- getArgs
  let cfg1 = maybe "shh" cs (args ^? ix 0) `getPuppetCfg` shhCfg
  let cfg2 = maybe "zsh" cs (args ^? ix 1) `getPuppetCfg` zshCfg

  -- TODO: this check doesn't save us from the situation when provided argument
  -- cause a program startup failure. We need to test this scenario
  traverse_
    ensureCmdExits
    [ "acquire_tty_wrapper",
      "stty",
      "xclip",
      (_pc_cmd cfg1),
      (_pc_cmd cfg2)
    ]

  stderrToFile "log.txt"
  openMuxLog "mux-log.txt"

  (pup1, pup1st) <- newPuppet Puppet1 cfg1
  (pup2, pup2st) <- newPuppet Puppet2 cfg2

  bracket
    configureStdinTty
    restoreStdinTty
    $ \_ -> do
      muxQueue <- newTQueueIO
      setSignalHandlers muxQueue
      forkReadUserInput muxQueue

      pup1pids <- maybe (_pup_startProcess pup1) pure (pup1st ^. ps_process)

      let mux =
            Mux
              muxQueue
              MuxEnv
                { _menv_puppets = pup1 :!: pup2
                }
              MuxState
                { _mst_puppetSt = pup1st {_ps_process = Just pup1pids} :!: pup2st,
                  _mst_currentPuppetIdx = Puppet1,
                  _mst_syncCwdP = Nothing,
                  _mst_keepAlive = False
                }

      muxLoop mux

  pure ()
