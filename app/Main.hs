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
import Foreign
import GHC.IO.Device
import qualified GHC.IO.FD as FD
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
bufSize = 1024

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
  -- ignore io errors
  _ <-
    tryIO $ do
      buff <- mallocForeignPtrBytes bufSize
      loop bufSize buff buff
  pure ()

forkPuppet ::
  PuppetIdx ->
  BTChan MuxCmd ->
  SomeMatcher ->
  GetCwd ->
  (Text -> Text) ->
  FilePath ->
  [String] ->
  IO (Puppet, PuppetState)
forkPuppet idx chan matcher getCwd cdCmd cmd args = do
  (master, slave) <- openPseudoTerminal
  masterH <- fdToHandle master
  slaveH <- fdToHandle slave
  pts <- getSlaveTerminalName master

  let startProcess = do
        (_, _, _, p) <-
          createProcess
            (proc cmd args)
              { std_in = UseHandle slaveH,
                std_out = UseHandle slaveH,
                std_err = UseHandle slaveH,
                new_session = True
              }
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
          _ps_cmdOutP = raceMatchersP accumCmdOutP,
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

openMuxLog :: IO ()
openMuxLog = do
  muxF <- openFile "mux-log.txt" AppendMode
  hSetBuffering muxF LineBuffering
  hSetBinaryMode muxF True
  putMVar muxLogFile muxF

setStdinToRaw :: IO ()
setStdinToRaw = do
  -- Disable input processing
  setRaw FD.stdin True
  setEcho FD.stdin False
  -- Disable output processing ~ stty -opost"
  attr <- getTerminalAttributes stdInput
  setTerminalAttributes stdInput (attr `withoutMode` ProcessOutput) WhenDrained

main :: IO ()
main = do
  stderrToFile "log.txt"
  openMuxLog

  setStdinToRaw

  muxChan <- newBTChanIO 10

  (pup1, pup1st) <-
    forkPuppet
      Puppet1
      muxChan
      (mkBracketMatcher "\ESC[1;36m\206\187\ESC[m  \ESC[1;32m" "\ESC[m  ")
      (GetCwdCommand "pwd")
      (\dir -> "cd \"" <> dir <> "\"")
      "shh"
      []

  (pup2, pup2st) <-
    forkPuppet
      Puppet2
      muxChan
      (mkSeqMatcher "\ESC[K\ESC[?2004h")
      GetCwdFromProcess
      (\dir -> " cd '" <> dir <> "'")
      "zsh"
      []

  syncTerminalSize (pup1 ^. pup_pts)
  syncTerminalSize (pup2 ^. pup_pts)

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
            { _mst_puppetSt = pup1st { _ps_process = Right pup1pids } :!: pup2st,
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

  pure ()
