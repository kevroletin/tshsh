{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Tshsh.Puppet
  ( GetCwd (..),
    PuppetCfg (..),
    pc_promptParser,
    pc_getCwdCmd,
    pc_mkCdCmd,
    pc_cleanPromptC,
    pc_restoreTuiC,
    Puppet (..),
    PuppetProcess (..),
    pp_handle,
    pp_pid,
    pp_inputH,
    pp_pts,
    pp_readThread,
    pup_idx,
    pup_promptParser,
    pup_getCwdCmd,
    pup_mkCdCmd,
    pup_startProcess,
    pup_initState,
    pup_cleanPromptC,
    pup_restoreTuiC,
    PuppetState (..),
    ps_idx,
    ps_parser,
    ps_clrScrParser,
    ps_mode,
    ps_currCmdOut,
    ps_prevCmdOut,
    ps_cmdOutP,
    ps_process,
    PuppetMode (..),
    SegmentedOutput (..),
    CmdResultOutput (..),
  )
where

import Control.Lens
import Data.BufferSlice (BufferSlice, SliceList (..))
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

newtype CmdResultOutput = CmdResultOutput { unCmdResultOutput :: Text }
  deriving (Show)

data GetCwd
  = GetCwdCommand Text
  | GetCwdFromProcess

data PuppetMode
  = PuppetModeTUI
  | PuppetModeRepl
  deriving (Eq, Ord, Show)

data PuppetProcess = PuppetProcess
  { _pp_handle :: ProcessHandle,
    _pp_pid :: ProcessID,
    _pp_inputH :: Handle,
    _pp_pts :: FilePath,
    _pp_readThread :: ThreadId
  }

$(makeLenses 'PuppetProcess)

data PuppetState = PuppetState
  { _ps_idx :: PuppetIdx,
    _ps_parser :: SomeMatcher,
    _ps_clrScrParser :: SomeMatcher,
    _ps_mode :: PuppetMode,
    _ps_currCmdOut :: SliceList,
    _ps_prevCmdOut :: SliceList,
    _ps_cmdOutP :: Program PuppetState BufferSlice SliceList IO,
    _ps_process :: Maybe PuppetProcess
  }

$(makeLenses 'PuppetState)

type PuppetAction i o = PuppetProcess -> ProgramAdaptCont' i o () CmdResultOutput ByteString IO

data PuppetCfg = PuppetCfg
  { _pc_cmd :: Text,
    _pc_cmdArgs :: [Text],
    _pc_promptParser :: SomeMatcher,
    _pc_getCwdCmd :: GetCwd,
    _pc_mkCdCmd :: Text -> Text,
    _pc_cleanPromptC :: forall pi po. PuppetAction pi po,
    _pc_restoreTuiC :: forall pi po. PuppetAction pi po
  }

$(makeLenses 'PuppetCfg)

data Puppet = Puppet
  { _pup_idx :: PuppetIdx,
    _pup_promptParser :: SomeMatcher,
    _pup_getCwdCmd :: GetCwd,
    _pup_mkCdCmd :: Text -> Text,
    _pup_startProcess :: IO PuppetProcess,
    _pup_initState :: PuppetState,
    _pup_cleanPromptC :: forall pi po. PuppetAction pi po,
    _pup_restoreTuiC :: forall pi po. PuppetAction pi po
  }

$(makeLenses 'Puppet)
