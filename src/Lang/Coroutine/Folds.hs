module Lang.Coroutine.Folds where

import Protolude
import Lang.Coroutine.Program

-- TODO: come up with consistent naming
foldProgram :: (s -> o -> s) -> s -> [i] -> Program i o () r -> (s, Maybe r)
foldProgram f0 res0 xs0 c0 =
  let feedInputAccumOut _ !res []     _ = (res, Nothing)
      feedInputAccumOut f !res (x:xs) c = loop f res xs (step (Just x) () c)

      loop f !res xs = \case
        Cont Nothing cont -> feedInputAccumOut f res xs cont
        Cont (Just o) cont -> loop f (f res o) xs (step Nothing () cont)
        Res o -> (res, Just o)

  in loop f0 res0 xs0 (step Nothing () c0)

accumFoldProgram :: forall i o r . [i] -> Program i o () r -> ([o], Maybe r)
accumFoldProgram is c = (reverse xs, r) where (xs, r) = foldProgram (\s o -> o : s) [] is c

eatResOutputs :: (s -> o -> s) -> s -> Res i o r -> (s, Either r (Program i o () r))
eatResOutputs f res0 r =
  let loop !res = \case
        Cont Nothing cont -> (res, Right cont)
        Cont (Just o) cont -> loop (f res o) (step Nothing () cont)
        Res o -> (res, Left o)

  in loop res0 r

eatOutputs :: (s -> o -> s) -> s -> Program i o () r -> (s, Either r (Program i o () r))
eatOutputs a b c = eatResOutputs a b (step Nothing () c)

accumOutputs :: forall i o r . Program i o () r -> ([o], Either r (Program i o () r))
accumOutputs c = (reverse xs, r) where (xs, r) = eatOutputs (\s o -> o : s) [] c

feedInputEatOut :: (s -> o -> s) -> s -> i -> Program i o () r -> (s, Either r (Program i o () r))
feedInputEatOut a b i c = eatResOutputs a b (step (Just i) () c)

feedInputAccumOut :: i -> Program i o () r -> ([o], Either r (Program i o () r))
feedInputAccumOut i c = (reverse xs, r) where (xs, r) = feedInputEatOut (\s o -> o : s) [] i c

feedInputEatOutSafe :: (s -> o -> s) -> s -> i -> Program i o () r -> (s, Either r (Program i o () r))
feedInputEatOutSafe f s i c =
  case eatOutputs f s c of
    (s', Left r) -> (s', Left r)
    (s', Right c') -> feedInputEatOut f s' i c'

feedInputAccumOutSafe :: i -> Program i o () r -> ([o], Either r (Program i o () r))
feedInputAccumOutSafe i c = (reverse xs, r) where (xs, r) = feedInputEatOutSafe (\s o -> o : s) [] i c

foldProgramM :: Monad m => (o -> m ()) -> m i -> Program i o () r -> m r
foldProgramM onOut getIn c0 =
  let feedInputAccumOut c = do i <- getIn
                               eatOutputs (step (Just i) () c)

      eatOutputs = \case
        Cont Nothing cont -> feedInputAccumOut cont
        Cont (Just o) cont -> do res' <- onOut o
                                 eatOutputs (step Nothing () cont)
        Res o -> pure o

  in eatOutputs (step Nothing () c0)

eatResOutputsM :: Monad m => (o -> m ()) -> Res i o r -> m (Either r (Program i o () r))
eatResOutputsM f r = do
  let loop = \case
        Cont Nothing cont -> pure (Right cont)
        Cont (Just o) cont -> do res' <- f o
                                 loop (step Nothing () cont)
        Res o -> pure (Left o)

  loop r

eatOutputsM :: Monad m => (o -> m ()) -> Program i o () r -> m (Either r (Program i o () r))
eatOutputsM f c = eatResOutputsM f (step Nothing () c)

feedInputM :: Monad m => (o -> m ()) -> i -> Program i o () r -> m (Either r (Program i o () r))
feedInputM f i c = eatResOutputsM f (step (Just i) () c)

feedInputSafeM :: Monad m => (o -> m ()) -> i -> Program i o () r -> m (Either r (Program i o () r))
feedInputSafeM f i c = do
  eatOutputsM f c >>= \case
    Left r -> pure (Left r)
    Right c' -> feedInputM f i c'
