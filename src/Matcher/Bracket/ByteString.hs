module Matcher.Bracket.ByteString
  ( mkMatcher,
    M.matcherStep,
    matchStr,
    Matcher,
    MatchResult (..),
  )
where

import qualified Data.ByteString as BS
import qualified Matcher.Bracket.Unboxed as M
import Matcher.Result
import Protolude

type Matcher = M.Matcher Word8

mkMatcher :: BS.ByteString -> BS.ByteString -> M.Matcher Word8
mkMatcher left right = M.mkMatcher (BS.unpack left) (BS.unpack right)

data MatchResult
  = Match
      { match_matcher :: Matcher,
        match_prev :: BS.ByteString,
        match_prompt :: BS.ByteString,
        match_rest :: BS.ByteString
      }
  | NoMatch {match_matcher :: Matcher}
  deriving (Eq, Show)

matchStr_ :: Matcher -> BS.ByteString -> Int -> BS.ByteString -> MatchResult
matchStr_ m orig pos str =
  case BS.uncons str of
    Nothing -> NoMatch m
    Just (h, t) ->
      case M.matcherStep m h of
        StepMatch offset m' ->
          Match m'
                (BS.take (1 + pos - offset) orig)
                (BS.take offset (BS.drop (1 + pos - offset) orig))
                t
        StepNoMatch m' -> matchStr_ m' orig (pos + 1) t

matchStr :: Matcher -> ByteString -> MatchResult
matchStr m str = matchStr_ m str 0 str
