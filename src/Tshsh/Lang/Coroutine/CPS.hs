{-# LANGUAGE TemplateHaskell #-}

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
    canProgressAfterTime,
    canProgressAfterTimeEv,
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
    waitInputInfC_,
    waitInputSecC_,
    waitInputDefC_,
    waitInputTimeC_,
    waitInputInfC,
    waitInputSecC,
    waitInputDefC,
    waitInputTimeC,
    waitSecC_,
    waitTimeC_,
    loc_waitInputInfC,
    loc_waitInputSecC,
    loc_waitInputDefC,
    loc_waitInputTimeC,
    loc_waitInputInfC_,
    loc_waitInputSecC_,
    loc_waitInputDefC_,
    loc_waitInputTimeC_,
    loc_waitSecC_,
    loc_waitTimeC_,
    adaptUnitStP,
    andThenP_,
  )
where

import Control.Lens
import Data.Strict.Tuple
import Data.Time
import Protolude
import qualified Tshsh.Constants as Const
import Tshsh.Lang.Coroutine.CPS.Folds
import Tshsh.Lang.Coroutine.CPS.Internal
import Tshsh.Lang.Coroutine.CPS.TH

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

loc_waitInputSecC_ :: Text -> Int -> ProgramCont_ st i o m r
loc_waitInputSecC_ loc sec cont = WaitInputTimeout loc (TimeoutRelative (fromIntegral sec)) $ \_ -> cont
{-# INLINE loc_waitInputSecC_ #-}

loc_waitInputSecC :: Text -> NominalDiffTime -> ProgramCont st i o m i r
loc_waitInputSecC loc sec cont = WaitInputTimeout loc (TimeoutRelative sec) cont
{-# INLINE loc_waitInputSecC #-}

loc_waitInputDefC_ :: Text -> ProgramCont_ st i o m r
loc_waitInputDefC_ loc cont = loc_waitInputSecC loc Const.defaultProgramTimeoutSec (\_ -> cont)
{-# INLINE loc_waitInputDefC_ #-}

loc_waitInputTimeC_ :: Text -> ProgramTimeout -> ProgramCont_ st i o m r
loc_waitInputTimeC_ loc time cont = WaitInputTimeout loc time $ \_ -> cont
{-# INLINE loc_waitInputTimeC_ #-}

loc_waitInputTimeC :: Text -> ProgramTimeout -> ProgramCont st i o m i r
loc_waitInputTimeC loc time cont = WaitInputTimeout loc time cont
{-# INLINE loc_waitInputTimeC #-}

loc_waitInputDefC :: Text -> ProgramCont st i o m i r
loc_waitInputDefC loc cont = loc_waitInputSecC loc Const.defaultProgramTimeoutSec cont
{-# INLINE loc_waitInputDefC #-}

loc_waitInputInfC_ :: Text -> ProgramCont_ st i o m r
loc_waitInputInfC_ loc cont = WaitInput loc $ \_ -> cont
{-# INLINE loc_waitInputInfC_ #-}

loc_waitInputInfC :: Text -> ProgramCont st i o m i r
loc_waitInputInfC loc cont = WaitInput loc cont
{-# INLINE loc_waitInputInfC #-}

loc_waitSecC_ :: Text -> NominalDiffTime -> ProgramCont_ st i o m r
loc_waitSecC_ loc sec cont = WaitTime loc (TimeoutRelative sec) cont
{-# INLINE loc_waitSecC_ #-}

loc_waitTimeC_ :: Text -> ProgramTimeout -> ProgramCont_ st i o m r
loc_waitTimeC_ loc time cont = WaitTime loc time cont
{-# INLINE loc_waitTimeC_ #-}

adaptUnitStP :: Program () i o m r -> Program st i o m r
adaptUnitStP = AdapterSt united
{-# INLINE adaptUnitStP #-}

andThenP_ :: Program st i o m () -> Program st i o m r -> Program st i o m r
andThenP_ a b = a `AndThen` const b
