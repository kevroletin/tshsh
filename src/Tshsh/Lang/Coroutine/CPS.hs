module Tshsh.Lang.Coroutine.CPS
  ( Program (..),
    ProgramEv,
    ProgramTimeout (..),
    StepEnv (..),
    pipe,
    unProgramEv,
    ProgramLike (..),
    Ev (..),
    ContResOut (..),
    _ContOut,
    _ResOut,
    stepUnsafe,
    stepInput,
    getNextTimeout,
    getNextTimeoutEv,
    EvWitness (..),
    matchEv,
    toEv,
    module Tshsh.Lang.Coroutine.CPS.Folds,
    ProgramSt,
    ProgramEvSt,
    ProgramCont,
    ProgramCont_,
    ifC,
    whenC,
    unlessC,
    liftP_,
    finishP,
    finishP_,
    waitInputC_,
    adaptUnitStP,
    andThenP_,
  )
where

import Control.Lens
import Data.Strict.Tuple
import Protolude
import Tshsh.Lang.Coroutine.CPS.Folds
import Tshsh.Lang.Coroutine.CPS.Internal

type ProgramSt st i o m r = Pair st (Program st i o m r)

type ProgramEvSt st i o m r = Pair st (ProgramEv 'Ev st i o m r)

type ProgramCont st i o m s r = (s -> Program st i o m r) -> Program st i o m r

type ProgramCont_ st i o m r = Program st i o m r -> Program st i o m r

ifC :: Bool -> (a -> b) -> (a -> b) -> (a -> b)
ifC True cont _ = cont
ifC False _ cont = cont

whenC :: Bool -> (a -> a) -> a -> a
whenC False _ cont = cont
whenC True act cont = act cont
{-# INLINE whenC #-}

unlessC :: Bool -> (a -> a) -> a -> a
unlessC True _ cont = cont
unlessC False act cont = act cont
{-# INLINE unlessC #-}

liftP_ :: m a -> Program st i o m r -> Program st i o m r
liftP_ act cont = Lift act $ const cont
{-# INLINE liftP_ #-}

finishP :: r -> Program st i o m r
finishP = Finish . Right
{-# INLINE finishP #-}

finishP_ :: Program st i o m ()
finishP_ = Finish (Right ())
{-# INLINE finishP_ #-}

waitInputC_ :: ProgramCont_ st i o m r
waitInputC_ cont = WaitInput $ \_ -> cont
{-# INLINE waitInputC_ #-}

adaptUnitStP :: Program () i o m r -> Program st i o m r
adaptUnitStP = AdapterSt united
{-# INLINE adaptUnitStP #-}

andThenP_ :: Program st i o m () -> Program st i o m r -> Program st i o m r
andThenP_ a b = a `AndThen` const b
