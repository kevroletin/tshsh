{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Puppet where

import Control.Lens
import qualified Data.ByteString as BS
import Matcher.ByteString
import Protolude
import System.Posix (ProcessID)
import System.Process (ProcessHandle)

data Puppet = Puppet
  { _pup_prompt :: BS.ByteString,
    _pup_parser :: Matcher,
    _pup_inputH :: Handle,
    _pup_readThread :: ThreadId,
    _pup_process :: ProcessHandle,
    _pup_pid :: ProcessID,
    _pup_pts :: FilePath,
    _pup_mkCdCmd :: Text -> Text
  }

$(makeLenses 'Puppet)
