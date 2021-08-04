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
import Data.Strict.Tuple
import Data.String.Conversions
import Foreign
import GHC.IO.Device
import qualified GHC.IO.FD as FD
import Matcher.ByteString
import Protolude hiding (hPutStrLn, log, tryIO)
import System.IO (BufferMode (..), hFlush, hGetBufSome, hPrint, hSetBuffering)
import System.IO.Unsafe
import System.Posix
import System.Posix.Signals.Exts
import System.Process
import Tshsh.Commands
import Tshsh.Muxer
import Tshsh.Puppet
import Prelude (String)
import qualified Data.Text.IO as T

logFile :: Handle
{-# NOINLINE logFile #-}
logFile = unsafePerformIO $ do
  f <- openFile "log.txt" AppendMode
  hSetBuffering f LineBuffering
  pure f

muxLogFile :: Handle
{-# NOINLINE muxLogFile #-}
muxLogFile = unsafePerformIO $ do
  f <- openFile "mux-log.txt" AppendMode
  hSetBuffering f LineBuffering
  pure f

log :: Text -> IO ()
log str = do
  T.hPutStr logFile str
  hFlush logFile

muxLog :: Show a => a -> IO ()
muxLog = hPrint muxLogFile

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

  (_, _, _, p) <-
    createProcess
      (proc cmd args)
        { std_in = UseHandle slaveH,
          std_out = UseHandle slaveH,
          std_err = UseHandle slaveH,
          new_session = True
        }
  (Just pid) <- getPid p
  putStrLn ("Started: " <> (show pid :: Text) <> "\r")

  readThread <- forkIO . readLoop masterH $ \str ->
    atomically . writeBTChan chan $ PuppetOutput idx str

  let clrScrParser = mkSeqMatcher "\ESC[H\ESC[2J"

  pure
    ( Puppet
        { _pup_idx = idx,
          _pup_promptParser = matcher,
          _pup_inputH = masterH,
          _pup_process = p,
          _pup_pid = pid,
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
          _ps_prevCmdOut = BufferSlice.listEmpty
        }
    )

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

  let mux =
        Mux
          MuxEnv
            { _menv_puppets = pup1 :!: pup2,
              _menv_logger = log
            }
          MuxState
            { _mst_puppetSt = pup1st :!: pup2st,
              _mst_currentPuppetIdx = Puppet1,
              _mst_currentProgram = Nothing
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

  _ <- waitForProcess (_pup_process pup1)
  _ <- waitForProcess (_pup_process pup2)

  pure ()
