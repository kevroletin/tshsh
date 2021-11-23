{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Muxer.Types
  ( TshshCfg (..),
    tsh_puppets,
    tsh_firstPuppetIdx,
    tsh_secondPuppetIdx,
    tsh_keepAlive,
    tsh_keyBindings,
    MuxEnv (..),
    menv_defaultPuppet,
    menv_puppets,
    menv_currentPuppet,
    menv_sigQueue,
    menv_outputAvailable,
    menv_inputAvailable,
    menv_progTimeout,
    MuxKeyCommands (..),
    MuxState (..),
    mst_puppets,
    mst_currentPuppetIdx,
    mst_prevPuppetIdx,
    mst_keepAlive,
    mst_inputParser,
    mst_prevCmdOut,
    mst_currentPuppet,
    mst_prevPuppet,
    mst_switchPupP,
    mst_readInputSt,
    Mux (..),
    mux_env,
    mux_st,
  )
where

import Control.Concurrent.STM (TQueue, TVar)
import Control.Lens
import qualified Data.ByteString as BS
import Protolude hiding (hPutStrLn, log, tryIO)
import Tshsh.Commands
import Tshsh.KeyParser
import Tshsh.Lang.Coroutine.CPS
import Tshsh.Puppet

data TshshCfg = TshshCfg
  { _tsh_puppets :: Map PuppetIdx PuppetCfg,
    _tsh_firstPuppetIdx :: PuppetIdx,
    _tsh_secondPuppetIdx :: PuppetIdx,
    _tsh_keyBindings :: [KeyAction MuxKeyCommands],
    _tsh_keepAlive :: Bool
  }

data MuxEnv = MuxEnv
  { _menv_puppets :: Map PuppetIdx PuppetCfg,
    _menv_defaultPuppet :: PuppetIdx,
    _menv_outputAvailable :: TVar (Set PuppetIdx),
    _menv_inputAvailable :: TVar Bool,
    _menv_sigQueue :: TQueue MuxSignal,
    _menv_progTimeout :: TVar Bool
  }

data MuxState = MuxState
  { _mst_puppets :: Map PuppetIdx PuppetState,
    _mst_currentPuppetIdx :: PuppetIdx,
    _mst_prevPuppetIdx :: PuppetIdx,
    _mst_switchPupP :: Maybe (ProgramEv 'Ev MuxState (PuppetIdx, StrippedCmdResult) (PuppetIdx, BS.ByteString) IO ()),
    _mst_keepAlive :: Bool,
    _mst_inputParser :: KeyParserState MuxKeyCommands,
    _mst_prevCmdOut :: StrippedCmdResult,
    _mst_readInputSt :: ReadLoopSt
  }

data Mux = Mux
  { _mux_env :: MuxEnv,
    _mux_st :: MuxState
  }

$(makeLenses 'TshshCfg)
$(makeLenses 'MuxState)
$(makeLenses 'MuxEnv)
$(makeLenses 'Mux)

mst_currentPuppet :: Lens' MuxState (Maybe PuppetState)
mst_currentPuppet f m =
  let ls = mst_puppets . at (m ^. mst_currentPuppetIdx)
   in (\x -> m & ls .~ x) <$> f (m ^. ls)

mst_prevPuppet :: Lens' MuxState (Maybe PuppetState)
mst_prevPuppet f m =
  let ls = mst_puppets . at (m ^. mst_prevPuppetIdx)
   in (\x -> m & ls .~ x) <$> f (m ^. ls)

menv_currentPuppet :: MuxState -> Lens' MuxEnv (Maybe PuppetCfg)
menv_currentPuppet st f env =
  let ls = menv_puppets . at (st ^. mst_currentPuppetIdx)
   in (\x -> env & ls .~ x) <$> f (env ^. ls)
