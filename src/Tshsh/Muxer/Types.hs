{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Muxer.Types
  ( MuxEnv (..),
    menv_puppets,
    menv_currentPuppet,
    menv_sortedPuppets,
    MuxKeyCommands (..),
    MuxState (..),
    mst_puppets,
    mst_currentPuppetIdx,
    mst_prevPuppetIdx,
    mux_env,
    mux_st,
    mux_queue,
    mst_keepAlive,
    mst_inputParser,
    mst_prevCmdOut,
    sortPup,
    sortPup_,
    mst_currentPuppet,
    mst_prevPuppet,
    mst_sortedPuppets,
    mst_syncCwdP,
    Mux (..),
  )
where

import Control.Concurrent.STM (TQueue)
import Control.Lens
import qualified Data.ByteString as BS
import Data.Strict.Tuple
import Protolude hiding (hPutStrLn, log, tryIO)
import Tshsh.Commands
import Tshsh.KeyParser
import Tshsh.Lang.Coroutine.CPS
import Tshsh.Puppet

newtype MuxEnv = MuxEnv
  { _menv_puppets :: Pair Puppet Puppet
  }

data MuxKeyCommands
  = MuxKeyCopyLastOut
  | MuxKeyEditLastOut
  | MuxKeySwitch
  deriving (Show)

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
    _mux_env :: MuxEnv,
    _mux_st :: MuxState
  }

$(makeLenses 'MuxState)
$(makeLenses 'MuxEnv)
$(makeLenses 'Mux)

pupIdx :: PuppetIdx -> Lens' (Pair a a) a
pupIdx Puppet1 f (a :!: b) = (:!: b) <$> f a
pupIdx Puppet2 f (a :!: b) = (a :!:) <$> f b

sortPup :: PuppetIdx -> Lens' (Pair a a) (Pair a a)
sortPup Puppet1 f (a :!: b) = f (a :!: b)
sortPup Puppet2 f (a :!: b) = (\(a' :!: b') -> b' :!: a') <$> f (b :!: a)

sortPup_ :: PuppetIdx -> Pair a a -> Pair a a
sortPup_ Puppet1 (a :!: b) = (a :!: b)
sortPup_ Puppet2 (a :!: b) = (b :!: a)

mst_currentPuppet :: Lens' MuxState (Maybe PuppetState)
mst_currentPuppet f m =
  let ls = mst_puppets . at (m ^. mst_currentPuppetIdx)
   in (\x -> m & ls .~ x) <$> f (m ^. ls)

mst_prevPuppet :: Lens' MuxState (Maybe PuppetState)
mst_prevPuppet f m =
  let ls = mst_puppets . at (m ^. mst_prevPuppetIdx)
   in (\x -> m & ls .~ x) <$> f (m ^. ls)

mst_sortedPuppets :: Lens' MuxState (Pair (Maybe PuppetState) (Maybe PuppetState))
mst_sortedPuppets f m =
  let a = mst_currentPuppet
      b = mst_prevPuppet
   in (\(a' :!: b') -> m & a .~ a' & b .~ b') <$> f (m ^. a :!: m ^. b)

menv_currentPuppet :: MuxState -> Lens' MuxEnv Puppet
menv_currentPuppet st f env =
  let ls = menv_puppets . pupIdx (st ^. mst_currentPuppetIdx)
   in (\x -> env & ls .~ x) <$> f (env ^. ls)

menv_sortedPuppets :: MuxState -> Lens' MuxEnv (Pair Puppet Puppet)
menv_sortedPuppets st f env =
  let ls = menv_puppets . sortPup (st ^. mst_currentPuppetIdx)
   in (\x -> env & ls .~ x) <$> f (env ^. ls)
