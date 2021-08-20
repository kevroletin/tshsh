module Tshsh.Lang.Coroutine.CPS.Folds
  ( eatOutputsM,
    feedInputM,
  )
where

import Data.Strict.Tuple
import Tshsh.Lang.Coroutine.CPS.Internal
import Protolude

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
