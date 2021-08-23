{-# LANGUAGE RankNTypes #-}
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
    ReadLoopSt (..),
    Puppet (..),
    PuppetProcess (..),
    pp_handle,
    pp_pid,
    pp_inputH,
    pp_pts,
    pp_dataAvailable,
    pp_readSliceSt,
    pup_idx,
    pup_cmd,
    pup_cmdArgs,
    pup_promptMatcher,
    pup_getCwdCmd,
    pup_mkCdCmd,
    pup_startProcess,
    pup_initState,
    pup_switchEnterHook,
    pup_switchExitHook,
    pup_cleanPromptP,
    PuppetState (..),
    ps_idx,
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

import Control.Concurrent.STM
import Control.Lens
import Data.Strict.Tuple.Extended ()
import Foreign
import Protolude
import System.Posix (ProcessID)
import System.Process (ProcessHandle)
import Tshsh.Commands
import Tshsh.Data.BufferSlice (BufferSlice, SliceList (..))
import Tshsh.Lang.Coroutine.CPS
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

data ReadLoopSt = ReadLoopSt
  { _rl_capacity :: Int,
    _rl_buff :: ForeignPtr Word8,
    _rl_dataPtr :: ForeignPtr Word8,
    _rl_fileHandle :: Handle
  }

data PuppetProcess = PuppetProcess
  { _pp_handle :: ProcessHandle,
    _pp_pid :: ProcessID,
    _pp_inputH :: Handle,
    _pp_pts :: FilePath,
    _pp_dataAvailable :: TVar Bool,
    _pp_readSliceSt :: ReadLoopSt
  }

$(makeLenses 'PuppetProcess)

data OutputParserSt = OutputParserSt
  { _op_mode :: PuppetMode,
    _op_promptMatcher :: StreamConsumer ByteString Int,
    _op_tuiModeMatcher :: StreamConsumer ByteString (Bool, Int),
    _op_currCmdOut :: RawCmdResult
  }

$(makeLenses 'OutputParserSt)

data PuppetState = PuppetState
  { _ps_idx :: PuppetIdx,
    _ps_outputParser :: ProgramEvSt OutputParserSt BufferSlice StrippedCmdResult IO,
    _ps_process :: Maybe PuppetProcess
  }

$(makeLenses 'PuppetState)

ps_mode :: Lens' PuppetState PuppetMode
ps_mode = ps_outputParser . _1 . op_mode

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

data Puppet = Puppet
  { _pup_idx :: PuppetIdx,
    _pup_cmd :: Text,
    _pup_cmdArgs :: [Text],
    _pup_promptMatcher :: StreamConsumer ByteString Int,
    _pup_getCwdCmd :: GetCwd,
    _pup_mkCdCmd :: Text -> Text,
    _pup_startProcess :: IO PuppetProcess,
    _pup_initState :: PuppetState,
    _pup_switchEnterHook :: IO (),
    _pup_switchExitHook :: IO (),
    -- + remove partially entered command
    -- + cause a shell to output a prompt
    -- + wait for a prompt
    _pup_cleanPromptP :: PuppetAction
  }

$(makeLenses 'Puppet)
