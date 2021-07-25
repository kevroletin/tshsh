module Matcher.Seq.Text
  ( mkMatcher,
    M.matcherStep,
    matchStr,
    Matcher,
    MatchResult (..),
  )
where

import Control.Lens ((^.))
import qualified Data.Text as T
import Matcher.Result
import qualified Matcher.Seq.Unboxed as M
import Protolude

type Matcher = M.Matcher Char

mkMatcher :: T.Text -> Matcher
mkMatcher str = M.mkMatcher (T.unpack str)

data MatchResult
  = Match
      { match_matcher :: Matcher,
        match_prev :: Text,
        match_rest :: Text
      }
  | NoMatch {match_matcher :: Matcher}
  deriving (Eq, Show)

matchStr_ :: Matcher -> T.Text -> Int -> T.Text -> MatchResult
matchStr_ m orig !pos str =
  case T.uncons str of
    Nothing -> NoMatch m
    Just (h, t) ->
      case M.matcherStep m h of
        StepMatch _ m' -> Match m' (T.take (1 + pos - m ^. M.mch_maxPos) orig) t
        StepNoMatch m' -> matchStr_ m' orig (pos + 1) t

matchStr :: Matcher -> T.Text -> MatchResult
matchStr m str = matchStr_ m str 0 str
