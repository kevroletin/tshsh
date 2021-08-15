{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Tshsh.Matcher.Base
  ( SomeMatcher (..),
    MatcherI (..),
    MatcherArrI (..),
    applySomeMatcher,
    matcherReset,
    matcherStep,
    matchStr,
    mkSeqMatcher,
    mkBracketMatcher,
    CanUnbox
  )
where

import qualified Data.Array.Unboxed
import Data.ListLike (ListLike)
import qualified Tshsh.Matcher.Bracket.Base as BrM
import Tshsh.Matcher.Result
import qualified Tshsh.Matcher.Seq.Base as SeqM
import Protolude
import Prelude (Show(..))

type CanUnbox c = Data.Array.Unboxed.IArray Data.Array.Unboxed.UArray c

class MatcherI m c a where
  matcherStepI :: (Eq c, CanUnbox c) => m c a -> c -> StepResult (m c a) a
  matcherResetI :: m c a -> m c a

class MatcherI m c a => MatcherArrI m arr c a | arr -> c where
  matchStrI :: (Eq c, CanUnbox c) => m c a -> arr -> MatchResult (m c a) arr a

data SomeMatcher arr c a where
  SomeMatcher :: (Show (m c a), MatcherArrI m arr c a) => m c a -> SomeMatcher arr c a

instance Show (SomeMatcher arr c a) where
  show (SomeMatcher m) = Prelude.show m

applySomeMatcher :: forall r arr c a. SomeMatcher arr c a -> (forall m. (Show (m c a), MatcherArrI m arr c a) => m c a -> r) -> r
applySomeMatcher (SomeMatcher m) f = f m
{-# INLINE applySomeMatcher #-}

instance (Eq c, CanUnbox c) => MatcherI BrM.BracketMatcher c a where
  matcherStepI = BrM.matcherStep
  matcherResetI = BrM.matcherReset

instance (Eq c, CanUnbox c) => MatcherI SeqM.SeqMatcher c a where
  matcherStepI = SeqM.matcherStep
  matcherResetI = SeqM.matcherReset

instance (Eq c, CanUnbox c, ListLike list c) => MatcherArrI SeqM.SeqMatcher list c a where
  matchStrI = SeqM.matchStr

instance (Eq c, CanUnbox c, ListLike list c) => MatcherArrI BrM.BracketMatcher list c a where
  matchStrI = BrM.matchStr

matcherReset :: (Eq c, CanUnbox c) => SomeMatcher list c a -> SomeMatcher list c a
matcherReset m0 = applySomeMatcher m0 (SomeMatcher . matcherResetI)
{-# INLINE matcherReset #-}

matcherStep :: (Eq c, CanUnbox c) => SomeMatcher list c a -> c -> StepResult (SomeMatcher list c a) a
matcherStep m0 c =
  applySomeMatcher
    m0
    ( \m -> SomeMatcher `mapStepResult` matcherStepI m c
    )
{-# INLINE matcherStep #-}

matchStr :: (Eq c, CanUnbox c, ListLike list c) => SomeMatcher list c a -> list -> MatchResult (SomeMatcher list c a) list a
matchStr m0 str =
  applySomeMatcher
    m0
    ( \m -> SomeMatcher `mapMatchResult` matchStrI m str
    )
{-# INLINE matchStr #-}

mkSeqMatcher :: (Eq c, CanUnbox c, ListLike list c, Show (SeqM.SeqMatcher c a)) => a -> list -> SomeMatcher list c a
mkSeqMatcher ret xs = SomeMatcher (SeqM.mkMatcher ret xs)
{-# INLINE mkSeqMatcher #-}

mkBracketMatcher :: (Eq c, CanUnbox c, ListLike list c, Show (BrM.BracketMatcher c a)) => a -> list -> list -> SomeMatcher list c a
mkBracketMatcher ret l r = SomeMatcher (BrM.mkMatcher ret l r)
{-# INLINE mkBracketMatcher #-}
