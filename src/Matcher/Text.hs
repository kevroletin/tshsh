module Matcher.Text
  ( SomeMatcher,
    MatchResult,
    matchStr,
    matcherStep,
    mkSeqMatcher,
    mkBracketMatcher,
  )
where

import qualified Matcher.Result as R
import qualified Matcher.Unboxed as U
import Protolude

type SomeMatcher = U.SomeMatcher Text Char

type MatchResult = R.MatchResult SomeMatcher Text

type StepResult = R.StepResult SomeMatcher

matcherStep :: SomeMatcher -> Char -> StepResult
matcherStep = U.matcherStep
{-# INLINE matcherStep #-}

matchStr :: SomeMatcher -> Text -> MatchResult
matchStr = U.matchStr
{-# INLINE matchStr #-}

mkSeqMatcher :: Text -> SomeMatcher
mkSeqMatcher = U.mkSeqMatcher
{-# INLINE mkSeqMatcher #-}

mkBracketMatcher :: Text -> Text -> SomeMatcher
mkBracketMatcher = U.mkBracketMatcher
{-# INLINE mkBracketMatcher #-}
