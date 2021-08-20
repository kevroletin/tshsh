{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Tshsh.Lang.Coroutine.CPS.Folds
  ( ContRes (..),
    _Cont,
    _Res,
    eatOutputsM,
    feedInputM,
  )
where

import Data.Strict.Tuple
import Protolude
import Tshsh.Lang.Coroutine.CPS.Internal
import Control.Lens

data ContRes st i o m where
  Cont :: Pair st (ProgramEv 'Ev st i o m) -> ContRes st i o m
  Res :: Pair st (Either Text ()) -> ContRes st i o m

$(makePrisms 'Res)

deriving instance (Show st, Show i, Show o) => Show (ContRes st i o m)

eatResOutputsM :: forall st i o m. Monad m => (o -> m ()) -> ContResOut st i o m -> m (ContRes st i o m)
eatResOutputsM f r = do
  let loop = \case
        ContNoOut cont -> pure (Cont cont)
        ContOut o cont -> do
          _ <- f o
          loop =<< stepOut cont
        ResOut o -> pure (Res o)
  loop r
{-# INLINE eatResOutputsM #-}

eatOutputsM :: forall st i o m prog. (ProgramLike prog st i o m, Monad m) => (o -> m ()) -> Pair st (prog st i o m) -> m (ContRes st i o m)
eatOutputsM f c = eatResOutputsM f =<< stepOut c
{-# INLINE eatOutputsM #-}

feedInputM :: forall st i o m. Monad m => (o -> m ()) -> i -> Pair st (ProgramEv 'Ev st i o m) -> m (ContRes st i o m)
feedInputM f i c = eatResOutputsM f =<< stepInput i c
{-# INLINE feedInputM #-}
