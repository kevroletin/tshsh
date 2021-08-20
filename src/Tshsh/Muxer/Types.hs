{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Muxer.Types
  ( MuxEnv (..),
    menv_puppets,
    menv_currentPuppet,
    menv_sortedPuppets,
    MuxState (..),
    mst_puppetSt,
    mst_currentPuppetIdx,
    mux_env,
    mux_st,
    mux_queue,
    mst_keepAlive,
    pupIdx,
    sortPup,
    sortPup_,
    mst_currentPuppet,
    mst_otherPuppet,
    mst_sortedPuppets,
    mst_syncCwdP,
    Mux (..),
  )
where

import Control.Lens
import qualified Data.ByteString as BS
import Data.Strict.Tuple
import Tshsh.Lang.Coroutine.CPS
import Protolude hiding (hPutStrLn, log, tryIO)
import Tshsh.Commands
import Tshsh.Puppet
import Control.Concurrent.STM (TQueue)

newtype MuxEnv = MuxEnv
  { _menv_puppets :: Pair Puppet Puppet
  }

data MuxState = MuxState
  { _mst_puppetSt :: Pair PuppetState PuppetState,
    _mst_currentPuppetIdx :: PuppetIdx,
    _mst_syncCwdP :: Maybe (Program () (PuppetIdx, StrippedCmdResult) (PuppetIdx, BS.ByteString) IO),
    _mst_keepAlive :: Bool
  }

data Mux = Mux { _mux_queue :: TQueue MuxCmd,
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

mst_currentPuppet :: Lens' MuxState PuppetState
mst_currentPuppet f m =
  let idx = m ^. mst_currentPuppetIdx
      ls = mst_puppetSt . pupIdx idx
   in (\x -> m & ls .~ x) <$> f (m ^. ls)

mst_otherPuppet :: Lens' MuxState PuppetState
mst_otherPuppet f m =
  let idx = nextPuppet (m ^. mst_currentPuppetIdx)
      ls = mst_puppetSt . pupIdx idx
   in (\x -> m & ls .~ x) <$> f (m ^. ls)

mst_sortedPuppets :: Lens' MuxState (Pair PuppetState PuppetState)
mst_sortedPuppets f m =
  let idx = m ^. mst_currentPuppetIdx
      ls = mst_puppetSt . sortPup idx
   in (\x -> m & ls .~ x) <$> f (m ^. ls)

menv_currentPuppet :: MuxState -> Lens' MuxEnv Puppet
menv_currentPuppet st f env =
  let ls = menv_puppets . pupIdx (st ^. mst_currentPuppetIdx)
   in (\x -> env & ls .~ x) <$> f (env ^. ls)

menv_sortedPuppets :: MuxState -> Lens' MuxEnv (Pair Puppet Puppet)
menv_sortedPuppets st f env =
  let ls = menv_puppets . sortPup (st ^. mst_currentPuppetIdx)
   in (\x -> env & ls .~ x) <$> f (env ^. ls)
