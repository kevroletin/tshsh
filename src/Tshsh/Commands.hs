{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Commands where

import qualified Data.ByteString as BS
import Protolude

data PuppetIdx = Puppet1 | Puppet2 deriving (Eq, Ord, Show, Enum)

nextPuppet :: PuppetIdx -> PuppetIdx
nextPuppet Puppet1 = Puppet2
nextPuppet Puppet2 = Puppet1

data MuxCmd
  = TermInput BS.ByteString
  | PuppetOutput PuppetIdx BS.ByteString
  | WindowResize
  | SwitchPuppet
  deriving (Show)

