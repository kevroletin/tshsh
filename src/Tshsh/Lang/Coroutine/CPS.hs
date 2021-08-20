module Tshsh.Lang.Coroutine.CPS
  ( eatOutputsM,
    feedInputM,
    Program (..),
    ProgramSt,
    ProgramCont,
    ProgramCont_,
    ContResOut (..),
    _ContOut,
    _ResOut,
    ContRes (..),
    _Cont,
    _Res,
    whenC,
    unlessC,
    liftP_,
    finishP,
    waitInputC_,
  )
where

import Tshsh.Lang.Coroutine.CPS.Folds
import Tshsh.Lang.Coroutine.CPS.Internal
import Protolude

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
