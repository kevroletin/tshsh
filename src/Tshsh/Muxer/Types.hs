{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Muxer.Types
  ( MuxEnv (..),
    menv_defaultPuppet,
    menv_puppets,
    menv_currentPuppet,
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
    mst_syncCwdP,
    Mux (..),
    mux_env,
    mux_st,
    mux_queue,
    mux_dataAvailable,
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

data MuxEnv = MuxEnv
  { _menv_puppets :: Map PuppetIdx Puppet,
    _menv_defaultPuppet :: Puppet
  }

data MuxState = MuxState
  { _mst_puppets :: Map PuppetIdx PuppetState,
    _mst_currentPuppetIdx :: PuppetIdx,
    _mst_prevPuppetIdx :: PuppetIdx,
    _mst_syncCwdP :: Maybe (ProgramEv 'Ev () (PuppetIdx, StrippedCmdResult) (PuppetIdx, BS.ByteString) IO),
    _mst_keepAlive :: Bool,
    _mst_inputParser :: KeyParserState MuxKeyCommands,
    _mst_prevCmdOut :: StrippedCmdResult
  }

data Mux = Mux
  { _mux_queue :: TQueue MuxCmd,
    _mux_dataAvailable :: TVar (Set PuppetIdx),
    _mux_env :: MuxEnv,
    _mux_st :: MuxState
  }

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

menv_currentPuppet :: MuxState -> Lens' MuxEnv (Maybe Puppet)
menv_currentPuppet st f env =
  let ls = menv_puppets . at (st ^. mst_currentPuppetIdx)
   in (\x -> env & ls .~ x) <$> f (env ^. ls)
