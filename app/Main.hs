module Main where

import Cli
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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

data DedicatedPuppets
  = PupRanger
  | PupZsh
  | PupVi
  | PupEmacs
  | PupShh
  deriving (Enum, Show)

-- TODO: this is very error-prone
zshIdx, shhIdx, rangerIdx, viIdx, pythonIdx, shIdx :: PuppetIdx
zshIdx = PuppetIdx 1
shhIdx = PuppetIdx 2
rangerIdx = PuppetIdx 3
viIdx = PuppetIdx 4
pythonIdx = PuppetIdx 5
shIdx = PuppetIdx 6

pupCfgs :: Map PuppetIdx PuppetCfg
pupCfgs =
  Map.fromList
    [ (zshIdx, zshCfg),
      (shhIdx, shhCfg),
      (rangerIdx, rangerCfg),
      (viIdx, viCfg),
      (pythonIdx, pythonCfg),
      (shIdx, shCfg)
    ]

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
          KeyPrefix
            "s"
            "switch"
            [ KeyAct "r" "ranger" (MuxKeySwitchPuppet rangerIdx),
              KeyAct "s" "shh" (MuxKeySwitchPuppet shhIdx),
              KeyAct "p" "python" (MuxKeySwitchPuppet pythonIdx),
              KeyAct "v" "vi" (MuxKeySwitchPuppet viIdx),
              KeyAct "z" "zsh" (MuxKeySwitchPuppet zshIdx)
            ]
        ]
    ]

main :: IO ()
main = do
  let (Right kb) = keyBindings

  (opts, args) <- parseArgs
  let cmd1 = maybe "shh" cs (args ^? ix 0)
      cmd2 = maybe "zsh" cs (args ^? ix 1)
      Just (idx1, cfg1) = find (\(_, cfg) -> _pc_cmd cfg == cmd1) $ Map.toList pupCfgs
      Just (idx2, cfg2) = find (\(_, cfg) -> _pc_cmd cfg == cmd2) $ Map.toList pupCfgs

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

  dataAvailable <- newTVarIO Set.empty

  bracket
    configureStdinTty
    restoreStdinTty
    $ \_ -> do
      sigQueue <- newTQueueIO
      setupSignalHandlers sigQueue
      forkReadUserInput sigQueue

      pup1st <- startPuppetProcess dataAvailable idx1 cfg1

      let mux =
            Mux
              MuxEnv
                { _menv_puppets = pupCfgs,
                  _menv_defaultPuppet = idx1,
                  _menv_dataAvailable = dataAvailable,
                  _menv_sigQueue = sigQueue
                }
              MuxState
                { _mst_puppets = Map.fromList [(idx1, pup1st)],
                  _mst_currentPuppetIdx = idx1,
                  _mst_prevPuppetIdx = idx2,
                  _mst_syncCwdP = Nothing,
                  _mst_keepAlive = False,
                  _mst_inputParser = kb,
                  _mst_prevCmdOut = StrippedCmdResult ""
                }

      muxLoop mux

  pure ()
