module Matcher.Text
  ( SomeMatcher,
    MatchResult,
    matchStr,
    matcherStep,
    mkSeqMatcher,
    mkBracketMatcher,
  )
where

import qualified Matcher.Bracket.Text
import qualified Matcher.Result as R
import qualified Matcher.Seq.Text
import qualified Matcher.Unboxed as U
import Protolude

type SomeMatcher = U.SomeMatcher Text Char

type MatchResult = R.MatchResult SomeMatcher Text

type StepResult = R.StepResult SomeMatcher

matcherStep :: SomeMatcher -> Char -> StepResult
matcherStep m0 c =
  U.applySomeMatcher
    m0
    ( \m -> U.SomeMatcher `R.mapStepResult` U.matcherStep m c
    )

matchStr :: SomeMatcher -> Text -> MatchResult
matchStr m0 str =
  U.applySomeMatcher
    m0
    ( \m -> U.SomeMatcher `R.mapMatchResult` U.matchStr m str
    )

mkSeqMatcher :: Text -> SomeMatcher
mkSeqMatcher = U.SomeMatcher . Matcher.Seq.Text.mkMatcher

mkBracketMatcher :: Text -> Text -> SomeMatcher
mkBracketMatcher l r = U.SomeMatcher (Matcher.Bracket.Text.mkMatcher l r)
