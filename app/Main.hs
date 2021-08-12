{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.BTChan
import Control.Exception.Safe (tryIO)
import Control.Lens
import Control.Monad
import Data.BufferSlice (BufferSlice (..))
import qualified Data.BufferSlice as BufferSlice
import qualified Data.ByteString as BS
import Data.Map as Map
import Data.Strict.Tuple.Extended
import Data.String.Conversions
import qualified Data.Text.IO as T
import Foreign
import Lang.Coroutine.CPS
import Matcher.ByteString
import Protolude hiding (tryIO)
import System.IO (BufferMode (..), hFlush, hGetBufSome, hPrint, hSetBinaryMode, hSetBuffering)
import System.IO.Unsafe
import System.Posix
import System.Posix.Signals.Exts
import System.Process
import Tshsh.Commands
import Tshsh.Muxer
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

readLoop :: Text -> Handle -> (BufferSlice -> IO ()) -> IO ()
readLoop name fromH act = do
  let loop !capacity buff0 dataPtr =
        if capacity < minBufSize
          then do
            buff <- mallocForeignPtrBytes bufSize
            loop bufSize buff buff
          else
            withForeignPtr dataPtr (\ptr -> hGetBufSome fromH ptr capacity) >>= \case
              n | n > 0 -> do
                act (BufferSlice buff0 dataPtr n)
                loop (capacity - n) buff0 (plusForeignPtr dataPtr n)
              _ -> pure ()
  res <-
    tryIO $ do
      buff <- mallocForeignPtrBytes bufSize
      loop bufSize buff buff
  case res of
    Left err -> hPutStrLn stderr (name <> " " <> show err)
    Right _ -> pure ()

newPuppet ::
  PuppetIdx ->
  BTChan MuxCmd ->
  PuppetCfg ->
  IO (Puppet, PuppetState)
newPuppet idx chan PuppetCfg {..} = do
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

        readThread <- forkIO . readLoop "[Read puppet output thread]" masterH $ \str ->
          atomically . writeBTChan chan $ PuppetOutput idx str

        hPutStrLn stderr ("Started: " <> (show pid :: Text))
        pure $
          PuppetProcess
            { _pp_handle = p,
              _pp_pid = pid,
              _pp_inputH = masterH,
              _pp_pts = pts,
              _pp_readThread = readThread
            }

  let clrScrParser = mkSeqMatcher "\ESC[H\ESC[2J"

  let puppetState =
        PuppetState
          { _ps_idx = idx,
            _ps_parser = _pc_promptParser,
            _ps_clrScrParser = clrScrParser,
            _ps_mode = PuppetModeRepl,
            _ps_currCmdOut = BufferSlice.listEmpty,
            _ps_prevCmdOut = BufferSlice.listEmpty,
            _ps_cmdOutP = raceMatchersP `Pipe` accumCmdOutP,
            _ps_process = Nothing
          }
  pure
    ( Puppet
        { _pup_idx = idx,
          _pup_promptParser = _pc_promptParser,
          _pup_getCwdCmd = _pc_getCwdCmd,
          _pup_mkCdCmd = _pc_mkCdCmd,
          _pup_startProcess = startProcess,
          _pup_initState = puppetState,
          _pup_switchEnterHook = _pc_switchEnterHook,
          _pup_switchExitHook = _pc_switchExitHook,
          _pup_cleanPromptC = _pc_cleanPromptC,
          _pup_restoreTuiC = _pc_restoreTuiC
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

restoreTtyState :: Text -> IO ()
restoreTtyState sttyState = do
  _ <- system ("stty " <> cs sttyState)
  pure ()

main :: IO ()
main = do
  stderrToFile "log.txt"
  openMuxLog "mux-log.txt"

  muxChan <- newBTChanIO 10

  let defCfg =
        PuppetCfg
          { _pc_cmd = "",
            _pc_cmdArgs = [],
            _pc_promptParser = mkBracketMatcher "sh-" "$ ",
            _pc_getCwdCmd = GetCwdFromProcess,
            _pc_mkCdCmd = (\dir -> "cd '" <> dir <> "'"),
            _pc_switchEnterHook = pure (),
            _pc_switchExitHook = pure (),
            _pc_cleanPromptC =
              ( \pp _ _ cont ->
                  liftP_ (BS.hPut (_pp_inputH pp) "\ETX") $ -- Ctrl-C
                    WaitInput $ \_ -> cont
              ),
            _pc_restoreTuiC =
              ( \pp _ _ ->
                  liftP_ (BS.hPut (_pp_inputH pp) "\ESC\f")
              )
          }
      shCfg =
        defCfg
          {
            _pc_cmd = "sh",
            _pc_cleanPromptC =
              ( \pp _ _ cont ->
                  liftP_
                    ( do
                        BS.hPut (_pp_inputH pp) "\NAK" -- Ctrl-U
                        BS.hPut (_pp_inputH pp) "\n"
                    )
                    $ WaitInput $ \_ -> cont
              )
          }
      pythonCfg =
        defCfg
          { _pc_cmd = "python3",
            _pc_promptParser = mkSeqMatcher ">>> ",
            _pc_mkCdCmd = (\dir -> "import os; os.chdir('" <> dir <> "')"),
            _pc_cleanPromptC =
              ( \pp _ _ cont ->
                  liftP_
                    ( do
                        BS.hPut (_pp_inputH pp) "\NAK" -- Ctrl-U
                        BS.hPut (_pp_inputH pp) "\n"
                    )
                    $ WaitInput $ \_ -> cont
              )
          }
      shhCfg =
        defCfg
          { _pc_cmd = "shh",
            _pc_promptParser = mkBracketMatcher "\ESC[1;36m\206\187\ESC[m  \ESC[1;32m" "\ESC[m  ",
            _pc_getCwdCmd = GetCwdCommand "pwd",
            _pc_mkCdCmd = (\dir -> "cd \"" <> dir <> "\""),
            _pc_switchEnterHook = BS.hPut stdout "\x1b[?2004l" -- disable bracket paste mode
          }
      zshCfg =
        defCfg
          { _pc_cmd = "zsh",
            _pc_promptParser = mkSeqMatcher "\ESC[K\ESC[?2004h"
          }
      rangerCfg =
        PuppetCfg
          { _pc_cmd = "ranger",
            _pc_cmdArgs = [],
            -- TODO: need a dummy matcher
            _pc_promptParser = mkSeqMatcher "\ESC[K\ESC[?2004h",
            _pc_getCwdCmd = GetCwdFromProcess,
            -- TODO: need a program here
            _pc_mkCdCmd = (\_ -> ""),
            _pc_cleanPromptC = (\_ _ _ cont -> cont),
            _pc_switchEnterHook = pure (),
            _pc_switchExitHook = pure (),
            _pc_restoreTuiC =
              ( \pp _ _ cont ->
                  liftP_
                    ( do
                        BS.hPut (_pp_inputH pp) "\ESC"
                        BS.hPut (_pp_inputH pp) "\f"
                        BS.hPut (_pp_inputH pp) "\f"
                    )
                    cont
              )
          }

  let cfg =
        Map.fromList
          [ ("python", pythonCfg),
            ("ranger", rangerCfg),
            ("shh", shhCfg),
            ("zsh", zshCfg),
            ("sh", shCfg)
          ]

  args <- getArgs
  let cfg1 = fromMaybe shhCfg $ join ((`Map.lookup` cfg) <$> (args ^? ix 0))
  let cfg2 = fromMaybe zshCfg $ join ((`Map.lookup` cfg) <$> (args ^? ix 1))

  (pup1, pup1st) <- newPuppet Puppet1 muxChan cfg1
  (pup2, pup2st) <- newPuppet Puppet2 muxChan cfg2

  origTtyState <- saveTtyState
  _ <- system "stty raw -echo isig susp ^Z intr '' eof '' quit '' erase '' kill '' eol '' eol2 '' swtch '' start '' stop '' rprnt '' werase '' lnext '' discard ''"

  _ <- installHandler windowChange (Catch (atomically $ writeBTChan muxChan WindowResize)) Nothing
  let suspendSig = atomically $ writeBTChan muxChan SwitchPuppet
  _ <- installHandler keyboardStop (Catch suspendSig) Nothing

  _ <- setStoppedChildFlag True
  let onChildSig :: SignalInfo -> IO ()
      onChildSig (SignalInfo _ _ NoSignalSpecificInfo) = pure ()
      onChildSig (SignalInfo _ _ SigChldInfo {..}) = do
        hPutStr stderr ("Sig> Child status: " <> show siginfoPid <> " -> " <> show siginfoStatus <> "\n" :: Text)
        atomically $ writeBTChan muxChan (ChildExited siginfoPid)
  _ <- installHandler processStatusChanged (CatchInfo onChildSig) Nothing

  _readThread <- forkIO $
    readLoop "[Read stdin thread]" stdin $ \str ->
      atomically . writeBTChan muxChan $ TermInput str

  pup1pids <-
    case pup1st ^. ps_process of
      Nothing -> _pup_startProcess pup1
      Just pids -> pure pids

  let mux =
        Mux
          MuxEnv
            { _menv_puppets = pup1 :!: pup2
            }
          MuxState
            { _mst_puppetSt = pup1st {_ps_process = Just pup1pids} :!: pup2st,
              _mst_currentPuppetIdx = Puppet1,
              _mst_syncCwdP = Nothing,
              _mst_keepAlive = False
            }

  let loop !env !st = do
        cmd <- atomically (readBTChan muxChan)
        muxLog cmd
        muxBody env st cmd >>= \case
          Just newSt -> loop env newSt
          Nothing -> pure ()
  loop (mux ^. mux_env) (mux ^. mux_st)

  restoreTtyState origTtyState

  pure ()
