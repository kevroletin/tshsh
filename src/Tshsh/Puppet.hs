{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Puppet
  ( GetCwd (..),
    Puppet (..),
    pup_idx,
    pup_promptParser,
    pup_inputH,
    pup_pts,
    pup_getCwdCmd,
    pup_mkCdCmd,
    PuppetState (..),
    ps_idx,
    ps_parser,
    ps_readThread,
    ps_clrScrParser,
    ps_mode,
    ps_currCmdOut,
    ps_prevCmdOut,
    ps_modeP,
    ps_process,
    PuppetMode (..),
    SegmentedOutput (..),
    CmdResultOutput (..),
  )
where

import Control.Lens
import Data.BufferSlice (BufferSlice, SliceList (..))
import Data.Strict.Tuple
import Lang.Coroutine.CPS
import Matcher.ByteString
import Protolude
import System.Posix (ProcessID)
import System.Process (ProcessHandle)
import Tshsh.Commands

data SegmentedOutput
  = Data BufferSlice
  | Prompt Int
  | TuiMode
  deriving (Show)

newtype CmdResultOutput = CmdResultOutput Text
  deriving (Show)

data GetCwd
  = GetCwdCommand Text
  | GetCwdFromProcess

data Puppet = Puppet
  { _pup_idx :: PuppetIdx,
    _pup_promptParser :: SomeMatcher,
    _pup_inputH :: Handle,
    _pup_pts :: FilePath,
    _pup_getCwdCmd :: GetCwd,
    _pup_mkCdCmd :: Text -> Text
  }

$(makeLenses 'Puppet)

data PuppetMode
  = PuppetModeTUI
  | PuppetModeRepl
  deriving (Eq, Ord, Show)

data PuppetState = PuppetState
  { _ps_idx :: PuppetIdx,
    _ps_parser :: SomeMatcher,
    _ps_readThread :: ThreadId,
    _ps_clrScrParser :: SomeMatcher,
    _ps_mode :: PuppetMode,
    _ps_currCmdOut :: SliceList,
    _ps_prevCmdOut :: SliceList,
    _ps_modeP :: Program PuppetState BufferSlice Text IO,
    _ps_process :: Either (IO (Pair ProcessHandle ProcessID)) (Pair ProcessHandle ProcessID)
  }

$(makeLenses 'PuppetState)
