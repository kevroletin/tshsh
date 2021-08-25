module Tshsh.Commands where

import Protolude
import System.Posix (ProcessID)
import Tshsh.Data.BufferSlice (BufferSlice)

newtype PuppetIdx = PuppetIdx Int deriving (Eq, Ord, Show, Enum)

data MuxCmd
  = TermInput BufferSlice
  | PuppetOutput PuppetIdx BufferSlice
  | WindowResize
  | SwitchPuppet
  | ChildExited ProcessID
  deriving (Show)

data MuxKeyCommands
  = MuxKeyCopyLastOut
  | MuxKeyEditLastOut
  | MuxKeySwitch
  | MuxKeySwitchPuppet PuppetIdx
  deriving (Show)
