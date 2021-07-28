module Matcher.ByteString
  ( SomeMatcher,
    MatchResult,
    matchStr,
    mkSeqMatcher,
    mkBracketMatcher,
  )
where

import qualified Matcher.Bracket.ByteString
import qualified Matcher.Result as R
import qualified Matcher.Seq.ByteString
import qualified Matcher.Unboxed as M
import Protolude

type SomeMatcher = M.SomeMatcher ByteString Word8

type MatchResult = R.MatchResult SomeMatcher ByteString

matchStr :: SomeMatcher -> ByteString -> MatchResult
matchStr m0 str =
  M.applySomeMatcher
    m0
    ( \m -> M.SomeMatcher `R.mapMatcher` M.matchStr m str
    )

mkSeqMatcher :: ByteString -> SomeMatcher
mkSeqMatcher = M.SomeMatcher . Matcher.Seq.ByteString.mkMatcher

mkBracketMatcher :: ByteString -> ByteString -> SomeMatcher
mkBracketMatcher l r = M.SomeMatcher (Matcher.Bracket.ByteString.mkMatcher l r)
