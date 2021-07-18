{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}

module Lang.Coroutine.CPS.Folds
  ( evalProgramM,
    eatOutputsM,
    feedInputM,
  )
where

import Lang.Coroutine.CPS
import Data.Bifunctor (first)
import Protolude

evalProgramM :: forall st i o m r. Monad m => (o -> m ()) -> m i -> (st, Program st i o m r) -> m (st, Either Text r)
evalProgramM onOut getIn c0 =
  let feedInputAccumOutUnsafe c = do
        i <- getIn
        foldOutputs =<< step (Just i) c

      foldOutputs = \case
        ContO Nothing stCont -> feedInputAccumOutUnsafe stCont
        ContO (Just o) stCont -> do
          res' <- onOut o
          foldOutputs =<< step Nothing stCont
        ResO o -> pure o
   in foldOutputs =<< step Nothing c0

eatResOutputsM :: forall st i o m r. Monad m => (o -> m ()) -> ResO st i o m r -> m (Res st i o m r)
eatResOutputsM f r = do
  let loop = \case
        ContO Nothing cont -> pure (Cont cont)
        ContO (Just o) cont -> do
          res' <- f o
          loop =<< step Nothing cont
        ResO o -> pure (Res o)

  loop r

eatOutputsM :: forall st i o m r. Monad m => (o -> m ()) -> (st, Program st i o m r) -> m (Res st i o m r)
eatOutputsM f c = eatResOutputsM f =<< step Nothing c

feedInputUnsafeM :: forall st i o m r. Monad m => (o -> m ()) -> i -> (st, Program st i o m r) -> m (Res st i o m r)
feedInputUnsafeM f i c = eatResOutputsM f =<< step (Just i) c

feedInputM :: forall st i o m r. Monad m => (o -> m ()) -> i -> (st, Program st i o m r) -> m (Res st i o m r)
feedInputM f i c = do
  eatOutputsM f c >>= \case
    Cont c' -> feedInputUnsafeM f i c'
    Res o -> pure (Res o)
