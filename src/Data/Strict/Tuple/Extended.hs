module Data.Strict.Tuple.Extended
  (
    module T,
    Field1(..)
  )
  where

import qualified Data.Strict.Tuple as T
import Data.Strict.Tuple
import Control.Lens
import Data.Functor

instance Field1 (Pair a b) (Pair a' b) a a' where
  _1 f (a :!: b) = (:!: b) <$> f a

instance Field2 (Pair a b) (Pair a b') b b' where
  _2 f (a :!: b) = (a :!:) <$> f b
