module Tshsh.Lang.Coroutine.CPS
  ( Program (..),
    ProgramEv,
    unProgramEv,
    ProgramLike (..),
    Ev (..),
    ContResOut (..),
    _ContOut,
    _ResOut,
    stepUnsafe,
    stepInput,
    EvWitness (..),
    matchEv,
    module Tshsh.Lang.Coroutine.CPS.Folds,
    ProgramSt,
    ProgramEvSt,
    ProgramCont,
    ProgramCont_,
    whenC,
    unlessC,
    liftP_,
    finishP,
    waitInputC_,
  )
where

import Data.Strict.Tuple
import Protolude
import Tshsh.Lang.Coroutine.CPS.Folds
import Tshsh.Lang.Coroutine.CPS.Internal

type ProgramSt st i o m = Pair st (Program st i o m)

type ProgramEvSt st i o m = Pair st (ProgramEv 'Ev st i o m)

type ProgramCont st i o m s = (s -> Program st i o m) -> Program st i o m

type ProgramCont_ st i o m = Program st i o m -> Program st i o m

whenC :: Bool -> (a -> a) -> a -> a
whenC False _ cont = cont
whenC True act cont = act cont
{-# INLINE whenC #-}

unlessC :: Bool -> (a -> a) -> a -> a
unlessC True _ cont = cont
unlessC False act cont = act cont
{-# INLINE unlessC #-}

liftP_ :: m a -> Program st i o m -> Program st i o m
liftP_ act cont = Lift act $ const cont
{-# INLINE liftP_ #-}

finishP :: Program st i o m
finishP = Finish (Right ())
{-# INLINE finishP #-}

waitInputC_ :: ProgramCont_ st i o m
waitInputC_ cont = WaitInput $ \_ -> cont
{-# INLINE waitInputC_ #-}
