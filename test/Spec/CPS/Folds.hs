{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Helpers for executing Lang.Coroutine.Program programs
--
-- One difficulty with executing our DSL programs is that all program's outputs
-- must be consumed before feeding inputs. For safety feedInput* functions try to
-- consume program outputs first, then feed the input and then consume output
-- again. Unsafe versions of feedInput* doesn't consume output as a first step
-- and hence might throw an exception.
--
-- Executing a program requires providing inputs and handling outputs. Both can be
-- provided using pure functions or effectful functions. Effectful versions have
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
-- Provided by Lang.Coroutine.CPS.Folds
module Spec.CPS.Folds
  ( foldProgram,
    accumProgram,
    foldOutputs,
    accumOutputs,
    feedInputFoldOutputs,
    feedInputAccumOutputs,
    feedInputAccumOutputsUnsafe
  )
where

import Lang.Coroutine.CPS
import Protolude

foldProgram :: forall s st i o r m. Monad m => (s -> o -> s) -> s -> [i] -> (st, Program st i o m r) -> m (s, Res st i o m r)
foldProgram f res0 xs0 c0 =
  let
      feedInput' !res [] c = pure (res, Cont c)
      feedInput' !res (x : xs) c = loop res xs =<< step (Just x) c

      loop !res xs = \case
        ContO Nothing cont -> feedInput' res xs cont
        ContO (Just o) cont -> loop (f res o) xs =<< step Nothing cont
        (ResO r) -> pure (res, Res r)
   in loop res0 xs0 =<< step Nothing c0

accumProgram :: forall st i o r m. Monad m => [i] -> (st, Program st i o m r) -> m ([o], Res st i o m r)
accumProgram is c = first reverse <$> foldProgram (\s o -> o : s) [] is c

foldResOutputs :: forall s st i o r m. Monad m => (s -> o -> s) -> s -> ResO st i o m r -> m (s, Res st i o m r)
foldResOutputs f res0 r =
  let loop !res = \case
        ContO Nothing cont -> pure (res, Cont cont)
        ContO (Just o) cont -> loop (f res o) =<< step Nothing cont
        ResO o -> pure (res, Res o)
   in loop res0 r

foldOutputs :: forall s st i o r m. Monad m => (s -> o -> s) -> s -> (st, Program st i o m r) -> m (s, Res st i o m r)
foldOutputs a b c = foldResOutputs a b =<< step Nothing c

accumOutputs :: forall st i o r m. Monad m => (st, Program st i o m r) -> m ([o], Res st i o m r)
accumOutputs c = first reverse <$> foldOutputs (\s o -> o : s) [] c

feedInputFoldOutUnsafe ::
  forall s st i o r m.
  Monad m =>
  (s -> o -> s) ->
  s ->
  i ->
  (st, Program st i o m r) ->
  m (s, Res st i o m r)
feedInputFoldOutUnsafe a b i c = foldResOutputs a b =<< step (Just i) c

feedInputAccumOutputsUnsafe :: forall st i o r m. Monad m => i -> (st, Program st i o m r) -> m ([o], Res st i o m r)
feedInputAccumOutputsUnsafe i c = first reverse <$> feedInputFoldOutUnsafe (\s o -> o : s) [] i c

feedInputFoldOutputs :: forall s st i o r m. Monad m => (s -> o -> s) -> s -> i -> (st, Program st i o m r) -> m (s, Res st i o m r)
feedInputFoldOutputs f s i c =
  foldOutputs f s c >>= \case
    (s', Cont c') -> feedInputFoldOutUnsafe f s' i c'
    (s', Res o) -> pure (s', Res o)

feedInputAccumOutputs :: forall st i o r m. Monad m => i -> (st, Program st i o m r) -> m ([o], Res st i o m r)
feedInputAccumOutputs i c = first reverse <$> feedInputFoldOutputs (\s o -> o : s) [] i c
