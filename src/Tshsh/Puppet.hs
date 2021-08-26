{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Puppet
  ( GetCwd (..),
    PuppetCfg (..),
    pc_cmd,
    pc_cmdArgs,
    pc_promptMatcher,
    pc_getCwdCmd,
    pc_mkCdCmd,
    pc_switchEnterHook,
    pc_switchExitHook,
    pc_cleanPromptP,
    ReadLoopSt,
    PuppetProcess (..),
    pp_handle,
    pp_pid,
    pp_inputH,
    pp_pts,
    pp_readSliceSt,
    PuppetState (..),
    ps_idx,
    ps_cfg,
    ps_outputParser,
    ps_process,
    ps_mode,
    OutputParserSt (..),
    op_mode,
    op_promptMatcher,
    op_tuiModeMatcher,
    op_currCmdOut,
    PuppetMode (..),
    ShellModeAndOutput (..),
    RawCmdResult (..),
    StrippedCmdResult (..),
  )
where

import Control.Lens
import Data.Strict.Tuple.Extended ()
import Foreign
import Protolude
import System.Posix (ProcessID)
import System.Process (ProcessHandle)
import Tshsh.Commands
import Tshsh.Data.BufferSlice (BufferSlice, SliceList (..))
import Tshsh.Lang.Coroutine.CPS
import Tshsh.ReadLoop (ReadLoopSt)
import Tshsh.Stream

data ShellModeAndOutput
  = Data BufferSlice
  | Prompt Int
  | TuiMode Bool Int
  deriving (Show)

newtype RawCmdResult = RawCmdResult {unRawCmdResult :: SliceList}
  deriving (Show)

data StrippedCmdResult = StrippedCmdResult {unStrippedCmdResult :: ~Text}
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
    _pp_readSliceSt :: ReadLoopSt
  }

$(makeLenses 'PuppetProcess)

type PuppetAction = PuppetProcess -> Program () StrippedCmdResult ByteString IO

data PuppetCfg = PuppetCfg
  { _pc_cmd :: Text,
    _pc_cmdArgs :: [Text],
    _pc_promptMatcher :: StreamConsumer ByteString Int,
    _pc_getCwdCmd :: GetCwd,
    _pc_mkCdCmd :: Text -> Text,
    _pc_switchEnterHook :: IO (),
    _pc_switchExitHook :: IO (),
    _pc_cleanPromptP :: PuppetAction
  }

$(makeLenses 'PuppetCfg)

data OutputParserSt = OutputParserSt
  { _op_mode :: PuppetMode,
    _op_promptMatcher :: StreamConsumer ByteString Int,
    _op_tuiModeMatcher :: StreamConsumer ByteString (Bool, Int),
    _op_currCmdOut :: RawCmdResult
  }

$(makeLenses 'OutputParserSt)

data PuppetState = PuppetState
  { _ps_idx :: PuppetIdx,
    _ps_cfg :: PuppetCfg,
    _ps_outputParser :: ProgramEvSt OutputParserSt BufferSlice StrippedCmdResult IO,
    _ps_process :: PuppetProcess
  }

$(makeLenses 'PuppetState)

ps_mode :: Lens' PuppetState PuppetMode
ps_mode = ps_outputParser . _1 . op_mode
