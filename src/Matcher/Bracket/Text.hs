module Matcher.Bracket.Text
  ( mkMatcher,
    M.matcherStep,
    matchStr,
    Matcher,
    MatchResult,
  )
where

import qualified Data.Text as T
import qualified Matcher.Bracket.Unboxed as M
import qualified Matcher.Result as R
import qualified Matcher.Seq.Text as S
import Protolude

type Matcher = M.Matcher Char

mkMatcher :: Text -> Text -> M.Matcher Char
mkMatcher left right = M.mkMatcher (T.unpack left) (T.unpack right)

type MatchResult = R.MatchResult Matcher Text

matchStr :: Matcher -> Text -> MatchResult
matchStr m0 str0 =
  if offset0 >= 0
    then matchRight 0 m0 str0
    else case S.matchStr (M._bmch_left m0) str0 of
      R.Match m len prev rest ->
        matchRight
          (T.length prev)
          ( m0
              { M._bmch_left = m,
                M._bmch_leftMatchOffset = len
              }
          )
          rest
      R.NoMatch m -> R.NoMatch (m0 {M._bmch_left = m})
  where
    offset0 = M._bmch_leftMatchOffset m0
    matchRight pos m1 str =
      case S.matchStr (M._bmch_right m1) str of
        R.Match _ _ prev rest ->
          R.Match
            (M.matcherReset m0)
            (M._bmch_leftMatchOffset m1 + T.length prev)
            (T.take (pos + T.length prev) str0)
            rest
        R.NoMatch m ->
          R.NoMatch
            ( m0
                { M._bmch_right = m,
                  M._bmch_leftMatchOffset = offset0 + T.length str
                }
            )
