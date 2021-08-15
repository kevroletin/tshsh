module Tshsh.Matcher.Text
  ( SomeMatcher,
    MatchResult,
    matchStr,
    matcherStep,
    mkSeqMatcher,
    mkBracketMatcher,
    matcherReset
  )
where

import qualified Tshsh.Matcher.Result as R
import qualified Tshsh.Matcher.Base as B
import Protolude

type SomeMatcher = B.SomeMatcher Text Char

type MatchResult a = R.MatchResult (SomeMatcher a) Text a

type StepResult a = R.StepResult (SomeMatcher a) a

matcherReset :: SomeMatcher a -> SomeMatcher a
matcherReset = B.matcherReset
{-# INLINE matcherReset #-}

matcherStep :: SomeMatcher a -> Char -> StepResult a
matcherStep = B.matcherStep
{-# INLINE matcherStep #-}

matchStr :: SomeMatcher a -> Text -> MatchResult a
matchStr = B.matchStr
{-# INLINE matchStr #-}

mkSeqMatcher :: Show a => a -> Text -> SomeMatcher a
mkSeqMatcher = B.mkSeqMatcher
{-# INLINE mkSeqMatcher #-}

mkBracketMatcher :: Show a => a -> Text -> Text -> SomeMatcher a
mkBracketMatcher = B.mkBracketMatcher
{-# INLINE mkBracketMatcher #-}
