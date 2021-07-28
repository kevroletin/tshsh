module Matcher.Seq.Text
  ( mkMatcher,
    M.matcherStep,
    matchStr,
    Matcher,
    MatchResult,
  )
where

import qualified Data.Text as T
import qualified Matcher.Result as R
import Matcher.Seq.Unboxed (mch_nextCharUnsafe)
import qualified Matcher.Seq.Unboxed as M
import Protolude

type Matcher = M.Matcher Char

mkMatcher :: T.Text -> Matcher
mkMatcher str = M.mkMatcher (T.unpack str)

type MatchResult = R.MatchResult Matcher Text

matchStr_ :: Matcher -> T.Text -> Int -> T.Text -> MatchResult
matchStr_ m0 orig !pos str =
  case T.uncons str of
    Nothing -> R.NoMatch m0
    Just (h, t) ->
      case M.matcherStep m0 h of
        R.StepMatch _ m' -> R.Match m' (M._mch_maxPos m0) (T.take (1 + pos) orig) t
        R.StepNoMatch m' ->
          if M._mch_pos m' == 0
            then
              let c = mch_nextCharUnsafe m'
                  (skip, rest) = T.break (== c) t
               in matchStr_ m' orig (pos + 1 + T.length skip) rest
            else matchStr_ m' orig (pos + 1) t

matchStr :: Matcher -> Text -> MatchResult
matchStr m str = matchStr_ m str 0 str
