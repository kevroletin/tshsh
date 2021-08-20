{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

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
    ps_promptMatcher,
    ps_tuiModeMatcher,
    ps_mode,
    ps_currCmdOut,
    ps_prevCmdOut,
    ps_outputParser,
    ps_process,
    PuppetMode (..),
    ShellModeAndOutput (..),
    RawCmdResult (..),
    StrippedCmdResult (..),
  )
where

import Control.Lens
import Control.Concurrent.STM
import Tshsh.Data.BufferSlice (BufferSlice, SliceList (..))
import Tshsh.Lang.Coroutine.CPS
import Protolude
import System.Posix (ProcessID)
import System.Process (ProcessHandle)
import Tshsh.Commands
import Tshsh.Stream
import Foreign

data ShellModeAndOutput
  = Data BufferSlice
  | Prompt Int
  | TuiMode Bool Int
  deriving (Show)

newtype RawCmdResult = RawCmdResult { unRawCmdResult :: SliceList }
  deriving (Show)

newtype StrippedCmdResult = StrippedCmdResult { unStrippedCmdResult :: Text }
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

data PuppetState = PuppetState
  { _ps_idx :: PuppetIdx,
    _ps_promptMatcher :: StreamConsumer ByteString Int,
    _ps_tuiModeMatcher :: StreamConsumer ByteString (Bool, Int),
    _ps_mode :: PuppetMode,
    _ps_currCmdOut :: RawCmdResult,
    _ps_prevCmdOut :: RawCmdResult,
    _ps_outputParser :: ProgramEv 'Ev PuppetState BufferSlice RawCmdResult IO,
    _ps_process :: Maybe PuppetProcess
  }

$(makeLenses 'PuppetState)

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
