{-# LANGUAGE BangPatterns #-}

-- |
-- Helpers for executing Lang.Coroutine.Program programs
--
-- One difficulty with executing our DSL programs is that all program's outputs
-- must be consumed before feeding inputs. For safety feedInput* functions try
-- consume program outputs first, then feed the input and then consume output
-- again. Unsafe versions of feedInput* doesn't consume output as a first step
-- and hence might throw an exception.
--
-- Executing a program requires providing inputs and handling outputs. Both can be
-- provided using pure functions or effectful functions. Effectful functions have
-- M suffix.
--
-- Pure functions
--
-- Inputs are provides as a list of values (or a single value).
--
-- Outputs can be consumed in a fold-like style using an accumulator and a transition
-- function (like this: (s > a -> s) -> a). Or they can be accumulated into a list.
-- Functions which use the first style are called fold*, the second is for accum*
-- (e.g. foldOutputs/accumOutputs).
--
-- Effectful functions
--
-- Inputs are provides be effectful "getter" function (or as a single value).
-- Outputs are fed into effectful "consumer" function.
module Lang.Coroutine.Folds
  ( foldProgram,
    accumProgram,
    foldOutputs,
    accumOutputs,
    feedInputFoldOutputs,
    feedInputAccumOutputs,
    evalProgramM,
    eatOutputsM,
    feedInputM,
  )
where

import Lang.Coroutine.Program
import Protolude

foldProgram :: (s -> o -> s) -> s -> [i] -> Program i o () r -> (s, Either Text r)
foldProgram f0 res0 xs0 c0 =
  let feedInputAccumOutUnsafe _ !res [] _ = (res, Left "foldProgram: end of input")
      feedInputAccumOutUnsafe f !res (x : xs) c = loop f res xs (step (Just x) () c)

      loop f !res xs = \case
        ContO Nothing cont -> feedInputAccumOutUnsafe f res xs cont
        ContO (Just o) cont -> loop f (f res o) xs (step Nothing () cont)
        ResO o -> (res, o)
   in loop f0 res0 xs0 (step Nothing () c0)

accumProgram :: forall i o r. [i] -> Program i o () r -> ([o], Either Text r)
accumProgram is c = (reverse xs, r) where (xs, r) = foldProgram (\s o -> o : s) [] is c

foldResOutputs :: (s -> o -> s) -> s -> ResO i o r -> (s, Res i o r)
foldResOutputs f res0 r =
  let loop !res = \case
        ContO Nothing cont -> (res, Cont cont)
        ContO (Just o) cont -> loop (f res o) (step Nothing () cont)
        ResO o -> (res, Res o)
   in loop res0 r

foldOutputs :: (s -> o -> s) -> s -> Program i o () r -> (s, Res i o r)
foldOutputs a b c = foldResOutputs a b (step Nothing () c)

accumOutputs :: forall i o r. Program i o () r -> ([o], Res i o r)
accumOutputs c = (reverse xs, r) where (xs, r) = foldOutputs (\s o -> o : s) [] c

feedInputFoldOutUnsafe :: (s -> o -> s) -> s -> i -> Program i o () r -> (s, Res i o r)
feedInputFoldOutUnsafe a b i c = foldResOutputs a b (step (Just i) () c)

feedInputAccumOutUnsafe :: i -> Program i o () r -> ([o], Res i o r)
feedInputAccumOutUnsafe i c = (reverse xs, r) where (xs, r) = feedInputFoldOutUnsafe (\s o -> o : s) [] i c

feedInputFoldOutputs :: (s -> o -> s) -> s -> i -> Program i o () r -> (s, Res i o r)
feedInputFoldOutputs f s i c =
  case foldOutputs f s c of
    (s', Cont c') -> feedInputFoldOutUnsafe f s' i c'
    (s', Res o) -> (s', Res o)

feedInputAccumOutputs :: i -> Program i o () r -> ([o], Res i o r)
feedInputAccumOutputs i c = (reverse xs, r) where (xs, r) = feedInputFoldOutputs (\s o -> o : s) [] i c

evalProgramM :: Monad m => (o -> m ()) -> m i -> Program i o () r -> m (Either Text r)
evalProgramM onOut getIn c0 =
  let feedInputAccumOutUnsafe c = do
        i <- getIn
        foldOutputs (step (Just i) () c)

      foldOutputs = \case
        ContO Nothing cont -> feedInputAccumOutUnsafe cont
        ContO (Just o) cont -> do
          res' <- onOut o
          foldOutputs (step Nothing () cont)
        ResO o -> pure o
   in foldOutputs (step Nothing () c0)

eatResOutputsM :: Monad m => (o -> m ()) -> ResO i o r -> m (Res i o r)
eatResOutputsM f r = do
  let loop = \case
        ContO Nothing cont -> pure (Cont cont)
        ContO (Just o) cont -> do
          res' <- f o
          loop (step Nothing () cont)
        ResO o -> pure (Res o)

  loop r

eatOutputsM :: Monad m => (o -> m ()) -> Program i o () r -> m (Res i o r)
eatOutputsM f c = eatResOutputsM f (step Nothing () c)

feedInputUnsafeM :: Monad m => (o -> m ()) -> i -> Program i o () r -> m (Res i o r)
feedInputUnsafeM f i c = eatResOutputsM f (step (Just i) () c)

feedInputM :: Monad m => (o -> m ()) -> i -> Program i o () r -> m (Res i o r)
feedInputM f i c = do
  eatOutputsM f c >>= \case
    Cont c' -> feedInputUnsafeM f i c'
    Res o -> pure (Res o)
