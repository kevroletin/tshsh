module Tshsh.Tty
  ( saveStdinTtyState,
    configureStdinTty,
    restoreStdinTty,
    syncTtySize,
    jiggleTtySize,
  )
where

import Data.String.Conversions
import qualified Data.Text.IO as T
import Protolude
import qualified System.Console.Terminal.Size as TerminalSize
import System.Process
import Prelude (String)

saveStdinTtyState :: IO Text
saveStdinTtyState = do
  (_, Just sttyOut, _, _) <-
    createProcess
      (proc "stty" ["--save"])
        { std_in = Inherit,
          std_out = CreatePipe,
          std_err = CreatePipe,
          new_session = True
        }
  T.hGetContents sttyOut

configureStdinTty :: IO Text
configureStdinTty = do
  origTtyState <- saveStdinTtyState

  -- disable all the signale except for susp
  let sttySignals = ["intr", "eof", "quit", "erase", "kill", "eol", "eol2", "swtch", "start", "stop", "rprnt", "werase", "lnext", "discard"]
  _ <- callCommand ("stty raw -echo isig susp ^Z " <> mconcat [x <> " '' " | x <- sttySignals])
  pure origTtyState

restoreStdinTty :: Text -> IO ()
restoreStdinTty sttyState = do
  _ <- system ("stty " <> cs sttyState)
  pure ()

syncTtySize :: String -> IO ()
syncTtySize pts = do
  Just (TerminalSize.Window h w :: TerminalSize.Window Int) <- TerminalSize.size
  _ <- callCommand ("stty -F " <> pts <> " cols " <> show w <> " rows " <> show h)
  pure ()

jiggleTtySize :: String -> IO ()
jiggleTtySize pts = do
  Just (TerminalSize.Window h w :: TerminalSize.Window Int) <- TerminalSize.size
  _ <- callCommand ("stty -F " <> pts <> " cols " <> show (w + 1) <> " rows " <> show h)
  _ <- callCommand ("stty -F " <> pts <> " cols " <> show w <> " rows " <> show h)
  pure ()
