{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Lang.Coroutine.CPS.Folds
  ( ContRes (..),
    _Cont,
    _Res,
    eatOutputsM,
    feedInputM,
  )
where

import Control.Lens
import Data.Strict.Tuple
import Protolude
import Tshsh.Lang.Coroutine.CPS.Internal

data ContRes st i o m r where
  Cont :: Pair st (ProgramEv 'Ev st i o m r) -> ContRes st i o m r
  Res :: Pair st (Either Text r) -> ContRes st i o m r

$(makePrisms 'Res)

deriving instance (Show st, Show i, Show o, Show r) => Show (ContRes st i o m r)

eatResOutputsM :: forall st i o m r. Monad m => StepEnv -> (o -> m ()) -> ContResOut st i o m r -> m (ContRes st i o m r)
eatResOutputsM env f r = do
  let loop = \case
        ContNoOut cont -> pure (Cont cont)
        ContOut o cont -> do
          _ <- f o
          loop =<< stepOut env cont
        ResOut o -> pure (Res o)
  loop r
{-# INLINE eatResOutputsM #-}

eatOutputsM :: forall st i o m r prog. (ProgramLike prog st i o m r, Monad m) => StepEnv -> (o -> m ()) -> Pair st (prog st i o m r) -> m (ContRes st i o m r)
eatOutputsM env f c = eatResOutputsM env f =<< stepOut env c
{-# INLINE eatOutputsM #-}

feedInputM :: forall st i o m r. Monad m => StepEnv -> (o -> m ()) -> i -> Pair st (ProgramEv 'Ev st i o m r) -> m (ContRes st i o m r)
feedInputM env f i c = eatResOutputsM env f =<< stepInput env i c
{-# INLINE feedInputM #-}
