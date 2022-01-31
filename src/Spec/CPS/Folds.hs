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
  forall s st i o r m prog.
  (ProgramLike prog st i o m r, Monad m) =>
  StepEnv ->
  (s -> o -> s) ->
  s ->
  [i] ->
  Pair st (prog st i o m r) ->
  m (s, ContRes st i o m r)
foldProgram env f res0 xs0 c0 =
  let feedInput' !res [] c = pure (res, Cont c)
      feedInput' !res (x : xs) c = loop res xs =<< stepInput env x c

      loop !res xs = \case
        ContNoOut cont -> feedInput' res xs cont
        ContOut o cont -> loop (f res o) xs =<< stepOut env cont
        (ResOut r) -> pure (res, Res r)
   in loop res0 xs0 =<< stepOut env c0

accumProgram :: forall st i o m r prog. (ProgramLike prog st i o m r, Monad m) => StepEnv -> [i] -> Pair st (prog st i o m r) -> m ([o], ContRes st i o m r)
accumProgram env is c = first reverse <$> foldProgram env (\s o -> o : s) [] is c

foldResOutputs :: forall s st i o m r. Monad m => StepEnv -> (s -> o -> s) -> s -> ContResOut st i o m r -> m (s, ContRes st i o m r)
foldResOutputs env f res0 r =
  let loop !res = \case
        ContNoOut cont -> pure (res, Cont cont)
        ContOut o cont -> loop (f res o) =<< stepOut env cont
        ResOut o -> pure (res, Res o)
   in loop res0 r

foldOutputs ::
  forall s st i o m r prog.
  (ProgramLike prog st i o m r, Monad m) =>
  StepEnv ->
  (s -> o -> s) ->
  s ->
  Pair st (prog st i o m r) ->
  m (s, ContRes st i o m r)
foldOutputs env a b c = foldResOutputs env a b =<< stepOut env c

accumOutputs :: forall st i o m r. Monad m => StepEnv -> Pair st (Program st i o m r) -> m ([o], ContRes st i o m r)
accumOutputs env c = first reverse <$> foldOutputs env (\s o -> o : s) [] c

feedInputFoldOutputs :: forall s st i o m r. Monad m => StepEnv -> (s -> o -> s) -> s -> i -> Pair st (Program st i o m r) -> m (s, ContRes st i o m r)
feedInputFoldOutputs env f s i c =
  foldOutputs env f s c >>= \case
    (s', Cont c') -> foldResOutputs env f s' =<< stepInput env i c'
    (s', Res o) -> pure (s', Res o)

feedInputAccumOutputs :: forall st i o m r. Monad m => StepEnv -> i -> Pair st (Program st i o m r) -> m ([o], ContRes st i o m r)
feedInputAccumOutputs env i c = first reverse <$> feedInputFoldOutputs env (\s o -> o : s) [] i c

evalProgramM ::
  forall st i o m prog r.
  (ProgramLike prog st i o m r, Monad m) =>
  StepEnv ->
  (o -> m ()) ->
  m i ->
  Pair st (prog st i o m r) ->
  m (Pair st (Either Text r))
evalProgramM env onOut getIn c0 =
  let feedInputAccumOut c = do
        i <- getIn
        loop =<< stepInput env i c

      loop = \case
        ContNoOut stCont -> feedInputAccumOut stCont
        ContOut o stCont -> do
          _ <- onOut o
          loop =<< stepOut env stCont
        ResOut o -> pure o
   in loop =<< stepOut env c0
