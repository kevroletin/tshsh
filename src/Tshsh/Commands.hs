module Tshsh.Commands where

import Protolude
import System.Posix (ProcessID)

newtype PuppetIdx = PuppetIdx Int deriving (Eq, Ord, Show, Enum)

data MuxSignal
  = WindowResize
  | ChildExited ProcessID
  deriving (Show)

data MuxKeyCommands
  = MuxKeyCopyLastOut
  | MuxKeyEditLastOut
  | MuxKeySwitch
  | MuxKeySwitchPuppet PuppetIdx
  deriving (Show)
