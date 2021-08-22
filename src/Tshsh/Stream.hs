{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Stream
  ( StreamConsumer (..),
    ConsumerI (..),
    ConsumerResult (..),
    applyConsumer,
    consumerReset,
    consume,
    cs_consumer,
    cs_prev,
    cs_rest,
    cs_result,
    mapConsumerResult,
    _ConsumerFinish,
    _ConsumerContinue,
  )
where

import Control.Lens
import Protolude

class ConsumerI c str a where
  consumeI :: c str a -> str -> ConsumerResult (c str a) str a
  resetI :: c str a -> c str a

data StreamConsumer str a where
  StreamConsumer :: (Show (c str a), ConsumerI c str a) => c str a -> StreamConsumer str a

deriving instance Show (StreamConsumer str a)

applyConsumer :: forall str a r. StreamConsumer str a -> (forall c. (Show (c str a), ConsumerI c str a) => c str a -> r) -> r
applyConsumer (StreamConsumer c) f = f c
{-# INLINE applyConsumer #-}

consumerReset :: StreamConsumer str a -> StreamConsumer str a
consumerReset m0 = applyConsumer m0 (StreamConsumer . resetI)
{-# INLINE consumerReset #-}

consume :: StreamConsumer str a -> str -> ConsumerResult (StreamConsumer str a) str a
consume m0 str =
  applyConsumer
    m0
    ( \c -> StreamConsumer `mapConsumerResult` consumeI c str
    )
{-# INLINE consume #-}

data ConsumerResult c str a
  = ConsumerFinish
      { _cs_consumer :: c,
        _cs_prev :: str,
        _cs_rest :: str,
        _cs_result :: a
      }
  | ConsumerContinue {_cs_consumer :: c}
  deriving (Show)

mapConsumerResult :: (c -> m') -> ConsumerResult c str a -> ConsumerResult m' str a
mapConsumerResult f c = c {_cs_consumer = f x} where x = _cs_consumer c
{-# INLINE mapConsumerResult #-}

$(makeLenses 'ConsumerFinish)
$(makePrisms 'ConsumerContinue)
