module Main where

import Cli
import Control.Lens
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.String.Conversions
import Protolude
import ShellConfig
import System.Directory
import System.IO (BufferMode (..), hFlush, hSetBuffering)
import System.Posix
import Tshsh.KeyParser
import Tshsh.Muxer

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

data Puppets
  = PupZsh
  | PupShh
  | PupRanger
  | PupVi
  | PupPython
  | PupSh
  deriving (Enum, Show)

pupIdx :: Enum e => e -> PuppetIdx
pupIdx = PuppetIdx . fromEnum

pupCfgs :: Map PuppetIdx PuppetCfg
pupCfgs = Map.fromList [(pupIdx i, c) | (i, c) <- cfgs]
  where
    cfgs =
      [ (PupZsh, zshCfg),
        (PupShh, shhCfg),
        (PupRanger, rangerCfg),
        (PupVi, viCfg),
        (PupPython, pythonCfg),
        (PupSh, shCfg)
      ]

keyBindings :: [KeyAction MuxKeyCommands]
keyBindings =
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
          [ KeyAct "r" "ranger" (switchPuppet PupRanger),
            KeyAct "s" "shh" (switchPuppet PupShh),
            KeyAct "p" "python" (switchPuppet PupPython),
            KeyAct "v" "vi" (switchPuppet PupVi),
            KeyAct "z" "zsh" (switchPuppet PupZsh)
          ]
      ]
  ]
  where
    switchPuppet x = MuxKeySwitchPuppet (pupIdx x)

main :: IO ()
main = do
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

  tshshMain
    TshshCfg
      { _tsh_puppets = pupCfgs,
        _tsh_firstPuppetIdx = idx1,
        _tsh_secondPuppetIdx = idx2,
        _tsh_keyBindings = keyBindings,
        _tsh_keepAlive = False
      }
