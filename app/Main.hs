{-# LANGUAGE LambdaCase, ScopedTypeVariables, RecordWildCards, OverloadedStrings, BangPatterns, StrictData #-}

module Main where

import Control.Exception.Safe
import Control.Concurrent
import Control.Monad
import System.IO
import System.Process
import System.Posix.Terminal
import System.Posix
import Foreign
import Foreign.C
import GHC.IO.Device
import qualified GHC.IO.FD as FD
import System.Posix.Signals
import System.Posix.Signals.Exts
import System.Console.Terminal.Size
import Control.Concurrent.STM

import Data.String.Conversions
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE

import Data.Attoparsec.ByteString

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

  (_, _, _, p) <- createProcess (proc cmd args) { std_in = UseHandle slaveH,
                                                  std_out = UseHandle slaveH,
                                                  std_err = UseHandle slaveH,
                                                  new_session = True }

  pts <- getSlaveTerminalName master
  pure (p, masterH, pts)

data PuppetIdx = Puppet1 | Puppet2 deriving (Eq, Ord, Show, Enum)

nextPuppet Puppet1 = Puppet2
nextPuppet Puppet2 = Puppet1

data Puppet = Puppet { pup_prompt :: BS.ByteString,
                       pup_parser :: Result BS.ByteString,
                       pup_lastOutput :: Maybe BS.ByteString,
                       pup_inputH :: Handle,
                       pup_readThread :: ThreadId,
                       pup_process :: ProcessHandle,
                       pup_pid :: ProcessID,
                       pup_pts :: FilePath,
                       pup_mkCdCmd :: Text -> Text
                     }


readLoop :: Handle -> (BS.ByteString -> IO ()) -> IO ()
readLoop from act = do
    buf <- mallocBytes bufSize
    let loop = do
          hGetBufSome from buf bufSize >>= \case
            n | n > 0 -> do str <- BS.packCStringLen (buf, n)
                            act str
                            loop
            _ -> pure ()
    -- ignore io errors
    _ <- tryIO loop
    pure ()

forkPuppet :: PuppetIdx
           -> TChan MuxCmd
           -> C.ByteString
           -> (Text -> Text)
           -> FilePath
           -> [String]
           -> IO Puppet
forkPuppet idx chan prompt cdCmd cmd args = do
  (master, slave) <- openPseudoTerminal
  masterH <- fdToHandle master
  slaveH <- fdToHandle slave
  pts <- getSlaveTerminalName master

  (_, _, _, p) <- createProcess (proc cmd args) { std_in = UseHandle slaveH,
                                                  std_out = UseHandle slaveH,
                                                  std_err = UseHandle slaveH,
                                                  new_session = True }
  (Just pid) <- getPid p
  print pid

  readThread <- forkIO $ readLoop masterH $ \str -> atomically . writeTChan chan $ (PuppetOutput idx str)

  -- TODO: this is a slow parser
  let parser :: Parser BS.ByteString = BS.pack <$> manyTill anyWord8 (string prompt)
  pure $ Puppet { pup_prompt = prompt,
                  pup_parser = Partial (parse parser),
                  pup_lastOutput = Nothing,
                  pup_inputH = masterH,
                  pup_readThread = readThread,
                  pup_process = p,
                  pup_pid = pid,
                  pup_pts = pts,
                  pup_mkCdCmd = cdCmd
                }

data MuxCmd = TermInput (BS.ByteString)              -- TODO: We can break Unicode Here :(
            | PuppetOutput PuppetIdx (BS.ByteString)
            | WindowResize
            | SwitchPuppet
            deriving (Show)

data MuxState = MuxState { mux_puppets :: (Puppet, Puppet),
                           mux_currentPuppetIdx :: PuppetIdx
                         }

currentPuppet (MuxState (a, _) Puppet1) = a
currentPuppet (MuxState (_, b) Puppet2) = b

getPuppetsInOrder :: MuxState -> (Puppet, Puppet)
getPuppetsInOrder MuxState{..} =
  if mux_currentPuppetIdx == Puppet1
    then mux_puppets
    else let !(a, b) = mux_puppets in (b, a)

getProcessCwd :: ProcessID -> IO Text
getProcessCwd pid =
  T.strip . T.pack <$> readProcess "readlink" ["/proc/" <> show pid <> "/cwd"] []

muxBody :: MuxState -> MuxCmd -> IO MuxState
muxBody st@MuxState{..} (TermInput str) = do
  let pup@Puppet{..} = currentPuppet st
  BS.hPut pup_inputH str
  pure st
muxBody st@MuxState{..} (PuppetOutput puppetIdx str) = do
  if puppetIdx == mux_currentPuppetIdx
    then BS.hPut stdout str
     -- TODO: what to do with background puppet output? just ignore fore now
    else pure()
  pure st
muxBody st@MuxState{..} WindowResize = do
  let pup@Puppet{..} = currentPuppet st
  Just (Window h w :: Window Int) <- size
  -- TODO: link c code
  _ <- system ("stty -F " <> pup_pts <> " cols " <> show w <> " rows " <> show h)
  signalProcess windowChange pup_pid

  pure st
muxBody st0@MuxState{..} SwitchPuppet = do
  let st = st0 { mux_currentPuppetIdx = nextPuppet mux_currentPuppetIdx }

  let (currP, prevP) = getPuppetsInOrder st

  signalProcess keyboardSignal (pup_pid currP)
  currCwd <- getProcessCwd (pup_pid currP)
  prevCwd <- getProcessCwd (pup_pid prevP)
  when (currCwd /= prevCwd) $ do
    BS.hPut (pup_inputH currP) (cs $ pup_mkCdCmd currP prevCwd <> "\n")

  -- print mux_currentPuppetIdx
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
  pup1 <- forkPuppet Puppet1 muxChan "sh-4.4$" (\dir -> "import os; os.chdir(\"" <> dir <> "\")") "python" []
  pup2 <- forkPuppet Puppet2 muxChan "sh-4.4$" (\dir -> "cd '" <> dir <> "'") "zsh" []
  -- pup2 <- forkPuppet Puppet2 muxChan "$  tshsh" "zsh" []

  let mux = MuxState { mux_puppets = (pup1, pup2),
                       mux_currentPuppetIdx = Puppet1
                     }

  muxThread <- forkIO $ do let loop !st = do
                                  cmd <- atomically (readTChan muxChan)
                                  -- print cmd
                                  newSt <- muxBody st cmd
                                  loop newSt
                           loop mux

  installHandler windowChange (Catch (atomically $ writeTChan muxChan WindowResize)) Nothing

  -- switch to the other puppet here
  let suspendSig = atomically $ writeTChan muxChan SwitchPuppet
  installHandler keyboardStop (Catch suspendSig) Nothing
  -- let onChildStatusChange (SignalInfo pid _ info) = do
  --       putStrLn ("--- SIGCHILD ---: " <> show pid)
        -- interruptProcessGroupOf (pup_process pup1)
        -- interruptProcessGroupOf (pup_process pup2)
        -- case info of
        --   NoSignalSpecificInfo -> pure ()
        --   (SigChldInfo pid uid status) -> print ("pind: " <> show pid <> ": " <> show status)
            -- case status of
            --   Exited ExitCode
            --   Terminated Signal Bool
            --   Stopped Signal
  -- installHandler processStatusChanged (CatchInfo onChildStatusChange) Nothing

  readThread <- forkIO $ readLoop stdin $ \str -> atomically . writeTChan muxChan $ (TermInput str)


  -- pipe <- openFile "/home/behemoth/Scratch/haskell/tshsh/test.pipe" ReadMode
  -- _ <- forkIO $ forever $ do copyLoop pipe stdout id
  --                            threadDelay 1000

  waitForProcess (pup_process pup1)
  waitForProcess (pup_process pup2)

  pure ()
