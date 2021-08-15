module Tshsh.Lang.Coroutine.CPS.Folds
  ( evalProgramM,
    eatOutputsM,
    feedInputM,
    feedInputUnsafeM,
  )
where

import Data.Strict.Tuple
import Tshsh.Lang.Coroutine.CPS
import Protolude

evalProgramM :: forall st i o m. Monad m => (o -> m ()) -> m i -> Pair st (Program st i o m) -> m (Pair st (Either Text ()))
evalProgramM onOut getIn c0 =
  let feedInputAccumOutUnsafe c = do
        i <- getIn
        foldOutputs =<< step (Just i) c

      foldOutputs = \case
        ContOut Nothing stCont -> feedInputAccumOutUnsafe stCont
        ContOut (Just o) stCont -> do
          _ <- onOut o
          foldOutputs =<< step Nothing stCont
        ResOut o -> pure o
   in foldOutputs =<< step Nothing c0

eatResOutputsM :: forall st i o m. Monad m => (o -> m ()) -> ContResOut st i o m -> m (ContRes st i o m)
eatResOutputsM f r = do
  let loop = \case
        ContOut Nothing cont -> pure (Cont cont)
        ContOut (Just o) cont -> do
          _ <- f o
          loop =<< step Nothing cont
        ResOut o -> pure (Res o)

  loop r

eatOutputsM :: forall st i o m. Monad m => (o -> m ()) -> Pair st (Program st i o m) -> m (ContRes st i o m)
eatOutputsM f c = eatResOutputsM f =<< step Nothing c

feedInputUnsafeM :: forall st i o m. Monad m => (o -> m ()) -> i -> Pair st (Program st i o m) -> m (ContRes st i o m)
feedInputUnsafeM f i c = eatResOutputsM f =<< step (Just i) c

feedInputM :: forall st i o m. Monad m => (o -> m ()) -> i -> Pair st (Program st i o m) -> m (ContRes st i o m)
feedInputM f i c = do
  eatOutputsM f c >>= \case
    Cont c' -> feedInputUnsafeM f i c'
    Res o -> pure (Res o)
