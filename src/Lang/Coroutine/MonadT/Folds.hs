{-# LANGUAGE BangPatterns #-}

module Lang.Coroutine.MonadT.Folds
  ( evalProgramM,
    eatOutputsM,
    feedInputM,
  )
where

import Lang.Coroutine.MonadT
import Protolude

evalProgramM :: Monad m => (o -> m ()) -> m i -> Program i o () m r -> m (Either Text r)
evalProgramM onOut getIn c0 =
  let feedInputAccumOutUnsafe c = do
        i <- getIn
        foldOutputs =<< step (Just i) () c

      foldOutputs = \case
        ContOut Nothing cont -> feedInputAccumOutUnsafe cont
        ContOut (Just o) cont -> do
          _ <- onOut o
          foldOutputs =<< step Nothing () cont
        ResOut o -> pure o
   in foldOutputs =<< step Nothing () c0

eatResOutputsM :: Monad m => (o -> m ()) -> m (ContResOut i o m r) -> m (ContRes i o m r)
eatResOutputsM f r = do
  let loop = \case
        ContOut Nothing cont -> pure (Cont cont)
        ContOut (Just o) cont -> do
          _ <- f o
          loop =<< step Nothing () cont
        ResOut o -> pure (Res o)
  loop =<< r

eatOutputsM :: Monad m => (o -> m ()) -> Program i o () m r -> m (ContRes i o m r)
eatOutputsM f c = eatResOutputsM f (step Nothing () c)

feedInputUnsafeM :: Monad m => (o -> m ()) -> i -> Program i o () m r -> m (ContRes i o m r)
feedInputUnsafeM f i c = eatResOutputsM f (step (Just i) () c)

feedInputM :: Monad m => (o -> m ()) -> i -> Program i o () m r -> m (ContRes i o m r)
feedInputM f i c = do
  eatOutputsM f c >>= \case
    Cont c' -> feedInputUnsafeM f i c'
    Res o -> pure (Res o)
