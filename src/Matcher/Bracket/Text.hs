module Matcher.Bracket.Text
  ( mkMatcher,
    matcherStep,
    matchStr,
    Matcher,
    MatchResult,
  )
where

import qualified Data.Text as T
import qualified Matcher.Bracket.Unboxed as U
import qualified Matcher.Result as R
import qualified Matcher.Seq.Text as S
import Protolude

{-# SPECIALIZE INLINE U.matcherStep :: U.Matcher Char -> Char -> R.StepResult (U.Matcher Char) #-}

{-# SPECIALIZE U.matchStr :: U.Matcher Char -> Text -> R.MatchResult (U.Matcher Char) Text #-}

type Matcher = U.Matcher Char

type MatchResult = R.MatchResult Matcher Text

type StepResult = R.StepResult Matcher

mkMatcher :: Text -> Text -> U.Matcher Char
mkMatcher = U.mkMatcher

matcherStep :: Matcher -> Char -> StepResult
matcherStep = U.matcherStep

matchStr :: Matcher -> Text -> MatchResult
matchStr = U.matchStr
