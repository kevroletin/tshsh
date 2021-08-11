{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Commands where

import Data.BufferSlice (BufferSlice)
import qualified Data.ByteString as BS
import Foreign.C.Types (CChar)
import GHC.ForeignPtr
import Protolude
import System.Posix (ProcessID)

data PuppetIdx = Puppet1 | Puppet2 deriving (Eq, Ord, Show, Enum)

nextPuppet :: PuppetIdx -> PuppetIdx
nextPuppet Puppet1 = Puppet2
nextPuppet Puppet2 = Puppet1

data MuxCmd
  = TermInput BufferSlice
  | PuppetOutput PuppetIdx BufferSlice
  | WindowResize
  | SwitchPuppet
  | ChildExited ProcessID
  deriving (Show)
