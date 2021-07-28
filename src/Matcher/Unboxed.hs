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
  )
where

import qualified Data.Array.Unboxed as U
import qualified Matcher.Bracket.Unboxed as MB
import qualified Matcher.Bracket.ByteString as MBBs
import qualified Matcher.Bracket.Text as MBT
import Matcher.Result
import qualified Matcher.Seq.Unboxed as MS
import qualified Matcher.Seq.ByteString as MSBs
import qualified Matcher.Seq.Text as MST
import Protolude

type CanUnbox a = U.IArray U.UArray a

class MatcherI m a where
  matcherStep  :: (Eq a, CanUnbox a) => m a -> a-> StepResult (m a)
  matcherReset :: m a -> m a

class MatcherI m a => MatcherArrI m arr a | arr -> a where
  matchStr     :: (Eq a, CanUnbox a) => m a -> arr -> MatchResult (m a) arr

data SomeMatcher arr a where
  SomeMatcher :: MatcherArrI m arr a => m a -> SomeMatcher arr a

applySomeMatcher :: forall r arr a. SomeMatcher arr a -> (forall m. MatcherArrI m arr a => m a -> r) -> r
applySomeMatcher (SomeMatcher m) f = f m

instance (Eq a, CanUnbox a) => MatcherI MB.Matcher a where
  matcherStep = MB.matcherStep
  matcherReset = MB.matcherReset

instance MatcherArrI MB.Matcher ByteString Word8 where
  matchStr = MBBs.matchStr

instance MatcherArrI MB.Matcher Text Char where
  matchStr = MBT.matchStr

instance (Eq a, CanUnbox a) => MatcherI MS.Matcher a where
  matcherStep = MS.matcherStep
  matcherReset = MS.matcherReset

instance MatcherArrI MS.Matcher ByteString Word8 where
  matchStr = MSBs.matchStr

instance MatcherArrI MS.Matcher Text Char where
  matchStr = MST.matchStr
