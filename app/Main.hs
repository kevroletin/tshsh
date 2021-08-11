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
bufSize = 64*1024

minBufSize :: Int
minBufSize = 64

newPuppet :: FilePath -> [String] -> IO (ProcessHandle, Handle, FilePath)
newPuppet cmd args = do
  (master, slave) <- openPseudoTerminal
  masterH <- fdToHandle master
  slaveH <- fdToHandle slave

  (_, _, _, p) <-
    createProcess
      (proc cmd args)
        { std_in = UseHandle slaveH,
          std_out = UseHandle slaveH,
          std_err = UseHandle slaveH,
          new_session = True
        }

  pts <- getSlaveTerminalName master
  pure (p, masterH, pts)

readLoop :: Handle -> (BufferSlice -> IO ()) -> IO ()
readLoop fromH act = do
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
  -- TODO: log io errors
  _ <-
    tryIO $ do
      buff <- mallocForeignPtrBytes bufSize
      loop bufSize buff buff
  pure ()

createPuppetAndTty ::
  PuppetIdx ->
  BTChan MuxCmd ->
  SomeMatcher ->
  GetCwd ->
  (Text -> Text) ->
  FilePath ->
  [String] ->
  IO (Puppet, PuppetState)
createPuppetAndTty idx chan matcher getCwd cdCmd cmd args = do
  (master, slave) <- openPseudoTerminal
  masterH <- fdToHandle master
  slaveH <- fdToHandle slave
  pts <- getSlaveTerminalName master

  let startProcess = do
        (_, _, _, p) <-
          createProcess
            -- To implement jobs control a process needs
            -- 1. to become a session leader
            -- 2. to acquire a controlling terminal so that it's children inherit the same terminal
            (proc "acquire_tty_wrapper" (cmd : args))
              { std_in = UseHandle slaveH,
                std_out = UseHandle slaveH,
                std_err = UseHandle slaveH
                -- new_session = True,
                -- create_group = False,
                -- aquire_tty = True -- this one is not implemented in rts that why we wrote an acquire_tty_wrapper
              }
        -- TODO: handle process startup failures
        (Just pid) <- getPid p
        hPutStrLn stderr ("Started: " <> (show pid :: Text))
        pure (p :!: pid)

  readThread <- forkIO . readLoop masterH $ \str ->
    atomically . writeBTChan chan $ PuppetOutput idx str

  let clrScrParser = mkSeqMatcher "\ESC[H\ESC[2J"

  pure
    ( Puppet
        { _pup_idx = idx,
          _pup_promptParser = matcher,
          _pup_inputH = masterH,
          _pup_pts = pts,
          _pup_getCwdCmd = getCwd,
          _pup_mkCdCmd = cdCmd
        },
      PuppetState
        { _ps_idx = idx,
          _ps_parser = matcher,
          _ps_readThread = readThread,
          _ps_clrScrParser = clrScrParser,
          _ps_mode = PuppetModeRepl,
          _ps_currCmdOut = BufferSlice.listEmpty,
          _ps_prevCmdOut = BufferSlice.listEmpty,
          _ps_cmdOutP = raceMatchersP `Pipe` accumCmdOutP,
          _ps_process = Left startProcess
        }
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

  (pup1, pup1st) <-
    createPuppetAndTty
      Puppet1
      muxChan
      (mkBracketMatcher "\ESC[1;36m\206\187\ESC[m  \ESC[1;32m" "\ESC[m  ")
      (GetCwdCommand "pwd")
      (\dir -> "cd \"" <> dir <> "\"")
      "shh"
      []

  (pup2, pup2st) <-
    createPuppetAndTty
      Puppet2
      muxChan
      (mkSeqMatcher "\ESC[K\ESC[?2004h")
      GetCwdFromProcess
      (\dir -> " cd '" <> dir <> "'")
      "zsh"
      []

  origTtyState <- saveTtyState
  _ <- system ("stty -F " <> (pup1 ^. pup_pts) <> " $(stty --save)")
  _ <- system ("stty -F " <> (pup2 ^. pup_pts) <> " $(stty --save)")
  syncTtySize (pup1 ^. pup_pts)
  syncTtySize (pup2 ^. pup_pts)
  _ <- system "stty raw -echo isig susp ^Z intr '' eof '' quit '' erase '' kill '' eol '' eol2 '' swtch '' start '' stop '' rprnt '' werase '' lnext '' discard ''"

  pup1pids <-
    case pup1st ^. ps_process of
      Left startProc -> startProc
      Right pids -> pure pids

  let mux =
        Mux
          MuxEnv
            { _menv_puppets = pup1 :!: pup2
            }
          MuxState
            { _mst_puppetSt = pup1st {_ps_process = Right pup1pids} :!: pup2st,
              _mst_currentPuppetIdx = Puppet1,
              _mst_syncCwdP = Nothing
            }

  _muxThread <- forkIO $ do
    let loop !env !st = do
          cmd <- atomically (readBTChan muxChan)
          muxLog cmd
          newSt <- muxBody env st cmd
          loop env newSt
    loop (mux ^. mux_env) (mux ^. mux_st)

  _ <- installHandler windowChange (Catch (atomically $ writeBTChan muxChan WindowResize)) Nothing

  -- switch to the other puppet here
  let suspendSig = atomically $ writeBTChan muxChan SwitchPuppet
  _ <- installHandler keyboardStop (Catch suspendSig) Nothing

  _readThread <- forkIO $ readLoop stdin $ \str -> atomically . writeBTChan muxChan $ TermInput str

  _ <- waitForProcess (pup1pids ^. _1)

  restoreTtyState origTtyState

  pure ()
