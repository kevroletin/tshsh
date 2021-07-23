{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Puppet
  ( GetCwd (..),
    Puppet (..),
    pup_idx,
    pup_promptParser,
    pup_inputH,
    pup_process,
    pup_pid,
    pup_pts,
    pup_getCwdCmd,
    pup_mkCdCmd,
    PuppetState (..),
    ps_idx,
    ps_parser,
    ps_readThread,
  )
where

import Control.Lens
import Matcher.ByteString
import Protolude
import System.Posix (ProcessID)
import System.Process (ProcessHandle)
import Tshsh.Commands

data GetCwd
  = GetCwdCommand Text
  | GetCwdFromProcess

data Puppet = Puppet
  { _pup_idx :: PuppetIdx,
    _pup_promptParser :: Matcher,
    _pup_inputH :: Handle,
    _pup_process :: ProcessHandle,
    _pup_pid :: ProcessID,
    _pup_pts :: FilePath,
    _pup_getCwdCmd :: GetCwd,
    _pup_mkCdCmd :: Text -> Text
  }

$(makeLenses 'Puppet)

data PuppetState = PuppetState
  { _ps_idx :: PuppetIdx,
    _ps_parser :: Matcher,
    _ps_readThread :: ThreadId
  }

$(makeLenses 'PuppetState)
