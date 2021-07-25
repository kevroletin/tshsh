{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Matcher.Unboxed
  ( Matcher (..),
    MatcherI (..),
    applyMatcher,
    mkSeqMatcher,
    mkBracketMatcher,
  )
where

import qualified Data.Array.Unboxed as U
import qualified Matcher.Bracket.Unboxed as MB
import Matcher.Result
import qualified Matcher.Seq.Unboxed as MS
import Protolude

type CanUnbox a = U.IArray U.UArray a

class MatcherI m a where
  matcherStep :: (Eq a, CanUnbox a) => m a -> a -> StepResult (m a)
  matcherReset :: m a -> m a

data Matcher a where
  Matcher :: MatcherI m a => m a -> Matcher a

applyMatcher :: forall r a. Matcher a -> (forall m. MatcherI m a => m a -> r) -> r
applyMatcher (Matcher m) f = f m

instance (Eq a, CanUnbox a) => MatcherI MB.Matcher a where
  matcherStep = MB.matcherStep
  matcherReset = MB.matcherReset

instance (Eq a, CanUnbox a) => MatcherI MS.Matcher a where
  matcherStep = MS.matcherStep
  matcherReset = MS.matcherReset

mkSeqMatcher :: (Eq a, CanUnbox a) => [a] -> Matcher a
mkSeqMatcher = Matcher . MS.mkMatcher

mkBracketMatcher :: (Eq a, CanUnbox a) => [a] -> [a] -> Matcher a
mkBracketMatcher l r = Matcher (MB.mkMatcher l r)
