module Matcher.ByteString
  ( SomeMatcher,
    MatchResult,
    matchStr,
    matcherStep,
    mkSeqMatcher,
    mkBracketMatcher,
  )
where

import qualified Matcher.Bracket.ByteString
import qualified Matcher.Result as R
import qualified Matcher.Seq.ByteString
import qualified Matcher.Unboxed as U
import Protolude

type SomeMatcher = U.SomeMatcher ByteString Word8

type MatchResult = R.MatchResult SomeMatcher ByteString

type StepResult = R.StepResult SomeMatcher

matcherStep :: SomeMatcher -> Word8 -> StepResult
matcherStep m0 c =
  U.applySomeMatcher
    m0
    ( \m -> U.SomeMatcher `R.mapStepResult` U.matcherStep m c
    )

matchStr :: SomeMatcher -> ByteString -> MatchResult
matchStr m0 str =
  U.applySomeMatcher
    m0
    ( \m -> U.SomeMatcher `R.mapMatchResult` U.matchStr m str
    )

mkSeqMatcher :: ByteString -> SomeMatcher
mkSeqMatcher = U.SomeMatcher . Matcher.Seq.ByteString.mkMatcher

mkBracketMatcher :: ByteString -> ByteString -> SomeMatcher
mkBracketMatcher l r = U.SomeMatcher (Matcher.Bracket.ByteString.mkMatcher l r)
