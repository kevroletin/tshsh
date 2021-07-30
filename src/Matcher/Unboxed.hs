{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Matcher.Unboxed
  ( SomeMatcher (..),
    MatcherI (..),
    MatcherArrI (..),
    applySomeMatcher,
    matcherStep,
    matchStr,
    mkSeqMatcher,
    mkBracketMatcher,
  )
where

import qualified Data.Array.Unboxed
import Data.ListLike (ListLike)
import qualified Matcher.Bracket.Unboxed as MB
import Matcher.Result
import qualified Matcher.Seq.Unboxed as MS
import Protolude

type CanUnbox a = Data.Array.Unboxed.IArray Data.Array.Unboxed.UArray a

class MatcherI m a where
  matcherStepI :: (Eq a, CanUnbox a) => m a -> a -> StepResult (m a)
  matcherResetI :: m a -> m a

class MatcherI m a => MatcherArrI m arr a | arr -> a where
  matchStrI :: (Eq a, CanUnbox a) => m a -> arr -> MatchResult (m a) arr

data SomeMatcher arr a where
  SomeMatcher :: MatcherArrI m arr a => m a -> SomeMatcher arr a

applySomeMatcher :: forall r arr a. SomeMatcher arr a -> (forall m. MatcherArrI m arr a => m a -> r) -> r
applySomeMatcher (SomeMatcher m) f = f m
{-# INLINE applySomeMatcher #-}

instance (Eq a, CanUnbox a) => MatcherI MB.Matcher a where
  matcherStepI = MB.matcherStep
  matcherResetI = MB.matcherReset

instance (Eq a, CanUnbox a) => MatcherI MS.Matcher a where
  matcherStepI = MS.matcherStep
  matcherResetI = MS.matcherReset

instance (Eq a, CanUnbox a, ListLike list a) => MatcherArrI MS.Matcher list a where
  matchStrI = MS.matchStr

instance (Eq a, CanUnbox a, ListLike list a) => MatcherArrI MB.Matcher list a where
  matchStrI = MB.matchStr

matcherStep :: (Eq a, CanUnbox a) => SomeMatcher list a -> a -> StepResult (SomeMatcher list a)
matcherStep m0 c =
  applySomeMatcher
    m0
    ( \m -> SomeMatcher `mapStepResult` matcherStepI m c
    )
{-# INLINE matcherStep #-}

matchStr :: (Eq a, CanUnbox a, ListLike list a) => SomeMatcher list a -> list -> MatchResult (SomeMatcher list a) list
matchStr m0 str =
  applySomeMatcher
    m0
    ( \m -> SomeMatcher `mapMatchResult` matchStrI m str
    )
{-# INLINE matchStr #-}

mkSeqMatcher :: (Eq a, CanUnbox a, ListLike list a) => list -> SomeMatcher list a
mkSeqMatcher = SomeMatcher . MS.mkMatcher
{-# INLINE mkSeqMatcher #-}

mkBracketMatcher :: (Eq a, CanUnbox a, ListLike list a) => list -> list -> SomeMatcher list a
mkBracketMatcher l r = SomeMatcher (MB.mkMatcher l r)
{-# INLINE mkBracketMatcher #-}
