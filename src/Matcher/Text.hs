module Matcher.Text
  ( SomeMatcher,
    MatchResult,
    matchStr,
    mkSeqMatcher,
    mkBracketMatcher,
  )
where

import qualified Matcher.Bracket.Text
import qualified Matcher.Result as R
import qualified Matcher.Seq.Text
import qualified Matcher.Unboxed as M
import Protolude

type SomeMatcher = M.SomeMatcher Text Char

type MatchResult = R.MatchResult SomeMatcher Text

matchStr :: SomeMatcher -> Text -> MatchResult
matchStr m0 str =
  M.applySomeMatcher
    m0
    ( \m -> M.SomeMatcher `R.mapMatcher` M.matchStr m str
    )

mkSeqMatcher :: Text -> SomeMatcher
mkSeqMatcher = M.SomeMatcher . Matcher.Seq.Text.mkMatcher

mkBracketMatcher :: Text -> Text -> SomeMatcher
mkBracketMatcher l r = M.SomeMatcher (Matcher.Bracket.Text.mkMatcher l r)
