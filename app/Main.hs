module Main where

import Cli
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Strict.Tuple.Extended
import Data.String.Conversions
import Protolude
import ShellConfig
import System.Directory
import System.IO (BufferMode (..), hFlush, hSetBuffering)
import System.Posix
import Tshsh.KeyParser
import Tshsh.Muxer
import Tshsh.Tty

stderrToFile :: Text -> IO ()
stderrToFile fName = do
  hFlush stderr
  logFileFd <- handleToFd =<< openFile (cs fName) AppendMode
  _ <- dupTo logFileFd stdError
  closeFd logFileFd
  hSetBuffering stderr LineBuffering

ensureCmdExits :: Text -> IO ()
ensureCmdExits cmd = do
  findExecutable (cs cmd) >>= \case
    Nothing -> do
      putStrLn ("Error: can't find executable " <> show cmd :: Text)
      exitFailure
    Just _ -> pure ()

keyBindings :: Either Text (KeyParserState MuxKeyCommands)
keyBindings =
  mkKeyParser
    [ -- Ctrl-z
      KeyAct "\SUB" "switch" MuxKeySwitch,
      -- Ctrl-x
      KeyPrefix
        "\CAN"
        "leader key"
        [ KeyAct "c" "copy previous output" MuxKeyCopyLastOut,
          KeyAct "e" "edit previous output" MuxKeyEditLastOut,
          KeyAct "z" "switch" MuxKeySwitch
        ]
    ]

main :: IO ()
main = do
  let (Right kb) = keyBindings

  (opts, args) <- parseArgs
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

  traverse_ openMuxLog (_cl_muxLog opts)
  stderrToFile (fromMaybe "/dev/null" $ _cl_log opts)

  (pup1, pup1st) <- newPuppet Puppet1 cfg1
  (pup2, pup2st) <- newPuppet Puppet2 cfg2

  bracket
    configureStdinTty
    restoreStdinTty
    $ \_ -> do
      muxQueue <- newTQueueIO
      setupSignalHandlers muxQueue
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
                  _mst_keepAlive = False,
                  _mst_inputParser = kb
                }

      muxLoop mux

  pure ()
