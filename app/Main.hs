{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe (tryIO)
import Control.Lens
import Control.Monad
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO as TLIO
import Foreign
import Foreign.C
import GHC.IO.Device
import qualified GHC.IO.FD as FD
import Matcher.ByteString
import Protolude hiding (hPutStrLn, log, tryIO)
import System.Console.Terminal.Size
import System.IO (BufferMode (..), hFlush, hGetBufSome, hPutStrLn, hSetBuffering)
import System.IO.Unsafe
import System.Posix
import System.Posix.Signals
import System.Posix.Signals.Exts
import System.Posix.Terminal
import System.Process
import Prelude (String)

logFile :: Handle
logFile = unsafePerformIO $ do
  f <- openFile "log.txt" AppendMode
  hSetBuffering f LineBuffering
  pure f

muxLogFile :: Handle
muxLogFile = unsafePerformIO $ do
  f <- openFile "mux-log.txt" AppendMode
  hSetBuffering f LineBuffering
  pure f

log :: Show a => a -> IO ()
log x = hPutStrLn logFile (show x)

muxLog :: Show a => a -> IO ()
muxLog x = hPutStrLn muxLogFile (show x)

bufSize = 1024

-- makeRaw
-- https://linux.die.net/man/3/cfmakeraw
-- termios_p->c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
-- termios_p->c_oflag &= ~OPOST;
-- termios_p->c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
-- termios_p->c_cflag &= ~(CSIZE | PARENB);
-- termios_p->c_cflag |= CS8;

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

data PuppetIdx = Puppet1 | Puppet2 deriving (Eq, Ord, Show, Enum)

nextPuppet Puppet1 = Puppet2
nextPuppet Puppet2 = Puppet1

data Puppet = Puppet
  { _pup_prompt :: BS.ByteString,
    _pup_parser :: Matcher,
    _pup_lastOutput :: Maybe BS.ByteString,
    _pup_inputH :: Handle,
    _pup_readThread :: ThreadId,
    _pup_process :: ProcessHandle,
    _pup_pid :: ProcessID,
    _pup_pts :: FilePath,
    _pup_mkCdCmd :: Text -> Text
  }

$(makeLenses 'Puppet)

readLoop :: Handle -> (BS.ByteString -> IO ()) -> IO ()
readLoop from act = do
  buf <- mallocBytes bufSize
  let loop = do
        hGetBufSome from buf bufSize >>= \case
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
  (Text -> Text) ->
  FilePath ->
  [String] ->
  IO Puppet
forkPuppet idx chan prompt cdCmd cmd args = do
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
    Puppet
      { _pup_prompt = prompt,
        _pup_parser = mkMatcher prompt,
        _pup_lastOutput = Nothing,
        _pup_inputH = masterH,
        _pup_readThread = readThread,
        _pup_process = p,
        _pup_pid = pid,
        _pup_pts = pts,
        _pup_mkCdCmd = cdCmd
      }

data MuxCmd
  = TermInput (BS.ByteString) -- TODO: We can break Unicode Here :(
  | PuppetOutput PuppetIdx (BS.ByteString)
  | WindowResize
  | SwitchPuppet
  deriving (Show)

data MuxState = MuxState
  { _mux_puppets :: (Puppet, Puppet),
    _mux_currentPuppetIdx :: PuppetIdx
  }

$(makeLenses 'MuxState)

currentPuppet :: Lens' MuxState Puppet
currentPuppet f (MuxState (a, b) idx) =
  case idx of
    Puppet1 -> (\a' -> MuxState (a', b) idx) <$> f a
    Puppet2 -> (\b' -> MuxState (a, b') idx) <$> f b

backgroundPuppet :: Lens' MuxState Puppet
backgroundPuppet f (MuxState (a, b) idx) =
  case idx of
    Puppet2 -> (\a' -> MuxState (a', b) idx) <$> f a
    Puppet1 -> (\b' -> MuxState (a, b') idx) <$> f b

sortedPuppets :: Lens' MuxState (Puppet, Puppet)
sortedPuppets f (MuxState (a, b) idx) =
  case idx of
    Puppet1 -> (\(a', b') -> MuxState (a', b') idx) <$> f (a, b)
    Puppet2 -> (\(a', b') -> MuxState (b', a') idx) <$> f (b, a)

getProcessCwd :: ProcessID -> IO Text
getProcessCwd pid =
  T.strip . T.pack <$> readProcess "readlink" ["/proc/" <> show pid <> "/cwd"] []

muxBody :: MuxState -> MuxCmd -> IO MuxState
muxBody st@MuxState {..} (TermInput str) = do
  let pup@Puppet {..} = st ^. currentPuppet
  BS.hPut _pup_inputH str
  pure st
muxBody st@MuxState {..} (PuppetOutput puppetIdx str0) = do
  if puppetIdx == _mux_currentPuppetIdx
    then do
      BS.hPut stdout str0

      -- TODO: do we want to do parsing in a main loop? maybe do it asynchronously?
      let loop m str =
            case matchStr m str of
              NoMatch m' -> pure m'
              Match m' _ rest ->
                if BS.null rest
                  then pure m'
                  else do
                    log "Match!"
                    loop m' rest
      m' <- loop (st ^. currentPuppet . pup_parser) str0

      pure (st & currentPuppet . pup_parser .~ m')
    else -- TODO: what to do with background puppet output? just ignore fore now
      pure st
muxBody st@MuxState {..} WindowResize = do
  let pup@Puppet {..} = st ^. currentPuppet
  Just (Window h w :: Window Int) <- size
  -- TODO: link c code
  _ <- system ("stty -F " <> _pup_pts <> " cols " <> show w <> " rows " <> show h)
  signalProcess windowChange _pup_pid

  pure st
muxBody st0@MuxState {..} SwitchPuppet = do
  let st = st0 & mux_currentPuppetIdx %~ nextPuppet

  -- let (currP, prevP) = getPuppetsInOrder st
  let (currP, prevP) = st ^. sortedPuppets

  signalProcess keyboardSignal (_pup_pid currP)
  currCwd <- getProcessCwd (_pup_pid currP)
  prevCwd <- getProcessCwd (_pup_pid prevP)
  when (currCwd /= prevCwd) $ do
    BS.hPut (_pup_inputH currP) (cs $ _pup_mkCdCmd currP prevCwd <> "\n")

  -- print _mux_currentPuppetIdx
  -- TODO: we should parse Unicode, otherwise we can break it on switch
  -- TODO: redraw last prompt

  -- TODO: Trying to dial with bracketed paste mode
  -- ghci doesn't work with bracketed paste mode
  -- for codes see https://cirw.in/blog/bracketed-paste
  BS.hPut stdout ("\x1b[?2004l" :: BS.ByteString)
  -- probably need to dump everything a pup dumps after initialization
  -- cause it set's up a terminal.
  -- We set raw mode on master and don't care about line discipline
  -- We deal with signals in the other place
  -- We just ignored flow control for now
  -- Now the problem that a puppet sets some parameters on it's virtual tty and
  -- we should set it to the main termina

  pure st

-- TODO: how to link c code to our project ?
-- TODO: can you build (lazy) byte string from Ptr Char?
-- TODO: learn about bytestring memory management
-- TODO: puppets become session leaders and
--       + don't die together
--       + `ps` shows all user processes (tmux doesn't have this drawback)
main :: IO ()
main = do
  setRaw FD.stdin True
  setEcho FD.stdin False

  muxChan <- newTChanIO
  -- TODO: set tty size during creation to avoid any races
  atomically $ writeTChan muxChan WindowResize

  -- pup1 <- forkPuppet Puppet1 muxChan "sh-4.4$" (\dir -> ":cd " <> dir) "shh" []
  pup1 <- forkPuppet Puppet1 muxChan ">>>" (\dir -> "import os; os.chdir(\"" <> dir <> "\")") "python" []
  -- pup2 <- forkPuppet Puppet2 muxChan "⚡" (\dir -> "cd '" <> dir <> "'") "zsh" []
  pup2 <- forkPuppet Puppet2 muxChan "\nsh-4.4$" (\dir -> "cd '" <> dir <> "'") "sh" []
  -- pup2 <- forkPuppet Puppet2 muxChan "$  tshsh" "zsh" []

  let mux =
        MuxState
          { _mux_puppets = (pup1, pup2),
            _mux_currentPuppetIdx = Puppet1
          }

  muxThread <- forkIO $ do
    let loop !st = do
          cmd <- atomically (readTChan muxChan)
          muxLog cmd
          newSt <- muxBody st cmd
          loop newSt
    loop mux

  installHandler windowChange (Catch (atomically $ writeTChan muxChan WindowResize)) Nothing

  -- switch to the other puppet here
  let suspendSig = atomically $ writeTChan muxChan SwitchPuppet
  installHandler keyboardStop (Catch suspendSig) Nothing

  readThread <- forkIO $ readLoop stdin $ \str -> atomically . writeTChan muxChan $ (TermInput str)

  waitForProcess (_pup_process pup1)
  waitForProcess (_pup_process pup2)

  pure ()
