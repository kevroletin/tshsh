module Matcher.Seq.Text
  ( mkMatcher,
    U.matcherStep,
    matchStr,
    Matcher,
    MatchResult,
  )
where

import qualified Matcher.Result as R
import qualified Matcher.Seq.Unboxed as U
import Protolude

{-# SPECIALIZE INLINE U.matcherStep :: U.Matcher Char -> Char -> R.StepResult (U.Matcher Char) #-}

{-# SPECIALIZE U.matchStr :: U.Matcher Char -> Text -> R.MatchResult (U.Matcher Char) Text #-}

type Matcher = U.Matcher Char

type MatchResult = R.MatchResult Matcher Text

type StepResult = R.StepResult Matcher

mkMatcher :: Text -> Matcher
mkMatcher = U.mkMatcher

matchStr :: Matcher -> Text -> MatchResult
matchStr = U.matchStr

matcherStep :: Matcher -> Char -> StepResult
matcherStep = U.matcherStep
