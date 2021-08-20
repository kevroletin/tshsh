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
    evalProgramM,
  )
where

import Data.Strict.Tuple
import Protolude
import Tshsh.Lang.Coroutine.CPS

foldProgram ::
  forall s st i o m prog.
  (ProgramLike prog st i o m, Monad m) =>
  (s -> o -> s) ->
  s ->
  [i] ->
  Pair st (prog st i o m) ->
  m (s, ContRes st i o m)
foldProgram f res0 xs0 c0 =
  let feedInput' !res [] c = pure (res, Cont c)
      feedInput' !res (x : xs) c = loop res xs =<< stepInput x c

      loop !res xs = \case
        ContNoOut cont -> feedInput' res xs cont
        ContOut o cont -> loop (f res o) xs =<< stepOut cont
        (ResOut r) -> pure (res, Res r)
   in loop res0 xs0 =<< stepOut c0

accumProgram :: forall st i o m prog. (ProgramLike prog st i o m, Monad m) => [i] -> Pair st (prog st i o m) -> m ([o], ContRes st i o m)
accumProgram is c = first reverse <$> foldProgram (\s o -> o : s) [] is c

foldResOutputs :: forall s st i o m. Monad m => (s -> o -> s) -> s -> ContResOut st i o m -> m (s, ContRes st i o m)
foldResOutputs f res0 r =
  let loop !res = \case
        ContNoOut cont -> pure (res, Cont cont)
        ContOut o cont -> loop (f res o) =<< stepOut cont
        ResOut o -> pure (res, Res o)
   in loop res0 r

foldOutputs ::
  forall s st i o m prog.
  (ProgramLike prog st i o m, Monad m) =>
  (s -> o -> s) ->
  s ->
  Pair st (prog st i o m) ->
  m (s, ContRes st i o m)
foldOutputs a b c = foldResOutputs a b =<< stepOut c

accumOutputs :: forall st i o m. Monad m => Pair st (Program st i o m) -> m ([o], ContRes st i o m)
accumOutputs c = first reverse <$> foldOutputs (\s o -> o : s) [] c

feedInputFoldOutputs :: forall s st i o m. Monad m => (s -> o -> s) -> s -> i -> Pair st (Program st i o m) -> m (s, ContRes st i o m)
feedInputFoldOutputs f s i c =
  foldOutputs f s c >>= \case
    (s', Cont c') -> foldResOutputs f s' =<< stepInput i c'
    (s', Res o) -> pure (s', Res o)

feedInputAccumOutputs :: forall st i o m. Monad m => i -> Pair st (Program st i o m) -> m ([o], ContRes st i o m)
feedInputAccumOutputs i c = first reverse <$> feedInputFoldOutputs (\s o -> o : s) [] i c

evalProgramM ::
  forall st i o m prog.
  (ProgramLike prog st i o m, Monad m) =>
  (o -> m ()) ->
  m i ->
  Pair st (prog st i o m) ->
  m (Pair st (Either Text ()))
evalProgramM onOut getIn c0 =
  let feedInputAccumOut c = do
        i <- getIn
        loop =<< stepInput i c

      loop = \case
        ContNoOut stCont -> feedInputAccumOut stCont
        ContOut o stCont -> do
          _ <- onOut o
          loop =<< stepOut stCont
        ResOut o -> pure o
   in loop =<< stepOut c0
