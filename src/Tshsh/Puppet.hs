{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Puppet where

import Control.Lens
import qualified Data.ByteString as BS
import Matcher.ByteString
import Protolude
import System.Posix (ProcessID)
import System.Process (ProcessHandle)

import Lang.Coroutine.CPS
import Tshsh.Commands

data Puppet = Puppet
  { _pup_idx :: PuppetIdx,
    _pup_prompt :: BS.ByteString,
    _pup_inputH :: Handle,
    _pup_process :: ProcessHandle,
    _pup_pid :: ProcessID,
    _pup_pts :: FilePath,
    _pup_getCwdCmd :: Text,
    _pup_mkCdCmd :: Text -> Text
  }

$(makeLenses 'Puppet)

data PuppetState = PuppetState
  { _ps_idx :: PuppetIdx,
    _ps_parser :: Matcher,
    _ps_readThread :: ThreadId
  }

$(makeLenses 'PuppetState)
