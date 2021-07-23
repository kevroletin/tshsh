{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Muxer.Types
  ( MuxEnv (..),
    menv_puppets,
    menv_logger,
    MuxState (..),
    mst_puppetSt,
    mst_currentPuppetIdx,
    mst_currentProgram,
    Mux (..),
    mux_env,
    mux_st,
    pupIdx,
    sortPup,
    mst_currentPuppet,
    mst_backgroundPuppet,
    mst_sortedPuppets,
    menv_currentPuppet,
    menv_sortedPuppets,
  )
where

import Control.Lens
import qualified Data.ByteString as BS
import Lang.Coroutine.CPS
import Protolude hiding (hPutStrLn, log, tryIO)
import Tshsh.Commands
import Tshsh.Puppet

data MuxEnv = MuxEnv
  { _menv_puppets :: (Puppet, Puppet),
    _menv_logger :: Text -> IO ()
  }

type SyncCwdProgram = Program () (PuppetIdx, BS.ByteString) (PuppetIdx, BS.ByteString) IO ()

type SyncCwdProgramSt = ((), SyncCwdProgram)

data MuxState = MuxState
  { _mst_puppetSt :: (PuppetState, PuppetState),
    _mst_currentPuppetIdx :: PuppetIdx,
    _mst_currentProgram :: Maybe SyncCwdProgramSt
  }

data Mux = Mux {_mux_env :: MuxEnv, _mux_st :: MuxState}

$(makeLenses 'MuxState)
$(makeLenses 'MuxEnv)
$(makeLenses 'Mux)

pupIdx :: PuppetIdx -> Lens' (a, a) a
pupIdx Puppet1 = _1
pupIdx Puppet2 = _2

sortPup :: PuppetIdx -> Lens' (a, a) (a, a)
sortPup Puppet1 f (a, b) = f (a, b)
sortPup Puppet2 f (a, b) = (\(a', b') -> (b', a')) <$> f (b, a)

mst_currentPuppet :: Lens' MuxState PuppetState
mst_currentPuppet f m =
  let idx = m ^. mst_currentPuppetIdx
      ls = mst_puppetSt . pupIdx idx
   in (\x -> m & ls .~ x) <$> f (m ^. ls)

mst_backgroundPuppet :: Lens' MuxState PuppetState
mst_backgroundPuppet f m =
  let idx = nextPuppet (m ^. mst_currentPuppetIdx)
      ls = mst_puppetSt . pupIdx idx
   in (\x -> m & ls .~ x) <$> f (m ^. ls)

mst_sortedPuppets :: Lens' MuxState (PuppetState, PuppetState)
mst_sortedPuppets f m =
  let idx = m ^. mst_currentPuppetIdx
      ls = mst_puppetSt . sortPup idx
   in (\x -> m & ls .~ x) <$> f (m ^. ls)

menv_currentPuppet :: MuxState -> Lens' MuxEnv Puppet
menv_currentPuppet st f env =
  let ls = menv_puppets . pupIdx (st ^. mst_currentPuppetIdx)
   in (\x -> env & ls .~ x) <$> f (env ^. ls)

menv_sortedPuppets :: MuxState -> Lens' MuxEnv (Puppet, Puppet)
menv_sortedPuppets st f env =
  let ls = menv_puppets . sortPup (st ^. mst_currentPuppetIdx)
   in (\x -> env & ls .~ x) <$> f (env ^. ls)
