{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Commands where

import qualified Data.ByteString as BS
import Foreign.C.Types (CChar)
import GHC.ForeignPtr
import Protolude

data BufferSlice = BufferSlice
  { _bs_buffId :: {-# UNPACK #-} ForeignPtr CChar,
    _bs_slice :: {-# UNPACK #-} ForeignPtr CChar,
    _bs_length :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Show)

data PuppetIdx = Puppet1 | Puppet2 deriving (Eq, Ord, Show, Enum)

nextPuppet :: PuppetIdx -> PuppetIdx
nextPuppet Puppet1 = Puppet2
nextPuppet Puppet2 = Puppet1

data MuxCmd
  = TermInput BufferSlice
  | PuppetOutput PuppetIdx BufferSlice
  | WindowResize
  | SwitchPuppet
  deriving (Show)
