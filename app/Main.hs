{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe (tryIO)
import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import Data.String.Conversions
import Foreign
import GHC.IO.Device
import qualified GHC.IO.FD as FD
import Matcher.ByteString
import Protolude hiding (hPutStrLn, log, tryIO)
import System.IO (BufferMode (..), hGetBufSome, hPrint, hSetBuffering)
import System.IO.Unsafe
import System.Posix
import System.Posix.Signals.Exts
import System.Process
import Tshsh.Commands
import Tshsh.Muxer
import Tshsh.Puppet
import Prelude (String)

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

log :: Show a => a -> IO ()
log = hPrint logFile

muxLog :: Show a => a -> IO ()
muxLog = hPrint muxLogFile

bufSize :: Int
bufSize = 1024

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

readLoop :: Handle -> (BS.ByteString -> IO ()) -> IO ()
readLoop fromH act = do
  buf <- mallocBytes bufSize
  let loop =
        hGetBufSome fromH buf bufSize >>= \case
          n | n > 0 -> do
            str <- BS.packCStringLen (buf, n)
            act str
            loop
          _ -> pure ()
  -- ignore io errors
  _ <- tryIO loop
  pure ()

forkPuppet ::
  PuppetIdx ->
  TChan MuxCmd ->
  BS.ByteString ->
  Text ->
  (Text -> Text) ->
  FilePath ->
  [String] ->
  IO (Puppet, PuppetState)
forkPuppet idx chan prompt getCwd cdCmd cmd args = do
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
  putStrLn ("Started: " <> show pid :: Text)

  readThread <- forkIO . readLoop masterH $ \str ->
    atomically . writeTChan chan $ PuppetOutput idx str

  pure $
    ( Puppet
        { _pup_idx = idx,
          _pup_prompt = prompt,
          _pup_promptParser = mkMatcher prompt,
          _pup_inputH = masterH,
          _pup_process = p,
          _pup_pid = pid,
          _pup_pts = pts,
          _pup_getCwdCmd = getCwd,
          _pup_mkCdCmd = cdCmd
        },
      PuppetState
        { _ps_idx = idx,
          _ps_parser = mkMatcher prompt,
          _ps_readThread = readThread
        }
    )

-- https://linux.die.net/man/3/cfmakeraw input is available character by
-- character, echoing is disabled, and all special processing of terminal input
-- and output characters is disabled
termSetRawMode :: IODevice a => a -> IO ()
termSetRawMode f = do
  setRaw f True
  setEcho f False

main :: IO ()
main = do
  termSetRawMode FD.stdin

  muxChan <- newTChanIO
  -- TODO: set tty size during creation to avoid any races
  atomically $ writeTChan muxChan WindowResize

  (pup1, pup1st) <-
    forkPuppet
      Puppet1
      muxChan
      ">>>"
      "import os; os.getcwd()"
      (\dir -> "import os; os.chdir(\"" <> dir <> "\")")
      "python"
      []

  (pup2, pup2st) <-
    forkPuppet
      Puppet2
      muxChan
      "\nsh-4.4$"
      "pwd"
      (\dir -> "cd '" <> dir <> "'")
      "sh"
      []

  let mux =
        Mux
          MuxEnv
            { _menv_puppets = (pup1, pup2),
              _menv_logger = log
            }
          MuxState
            { _mst_puppetSt = (pup1st, pup2st),
              _mst_currentPuppetIdx = Puppet1,
              _mst_currentProgram = Nothing
            }

  _muxThread <- forkIO $ do
    let loop !env !st = do
          cmd <- atomically (readTChan muxChan)
          muxLog cmd
          newSt <- muxBody env st cmd
          loop env newSt
    loop (mux ^. mux_env) (mux ^. mux_st)

  _ <- installHandler windowChange (Catch (atomically $ writeTChan muxChan WindowResize)) Nothing

  -- switch to the other puppet here
  let suspendSig = atomically $ writeTChan muxChan SwitchPuppet
  _ <- installHandler keyboardStop (Catch suspendSig) Nothing

  _readThread <- forkIO $ readLoop stdin $ \str -> atomically . writeTChan muxChan $ TermInput str

  _ <- waitForProcess (_pup_process pup1)
  _ <- waitForProcess (_pup_process pup2)

  pure ()
