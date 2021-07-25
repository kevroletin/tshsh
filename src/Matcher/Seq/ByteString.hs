module Matcher.Seq.ByteString
  ( mkMatcher,
    M.matcherStep,
    matchStr,
    Matcher,
    MatchResult (..),
  )
where

import Control.Lens ((^.))
import qualified Data.ByteString as BS
import Matcher.Result
import qualified Matcher.Seq.Unboxed as M
import Protolude

type Matcher = M.Matcher Word8

mkMatcher :: BS.ByteString -> Matcher
mkMatcher str = M.mkMatcher (BS.unpack str)

data MatchResult
  = Match
      { match_matcher :: Matcher,
        match_prev :: BS.ByteString,
        match_rest :: BS.ByteString
      }
  | NoMatch {match_matcher :: Matcher}
  deriving (Eq, Show)

matchStr_ :: Matcher -> BS.ByteString -> Int -> BS.ByteString -> MatchResult
matchStr_ m orig !pos str =
  case BS.uncons str of
    Nothing -> NoMatch m
    Just (h, t) ->
      case M.matcherStep m h of
        StepMatch _ m' -> Match m' (BS.take (1 + pos - m ^. M.mch_maxPos) orig) t
        StepNoMatch m' -> matchStr_ m' orig (pos + 1) t

matchStr :: Matcher -> ByteString -> MatchResult
matchStr m str = matchStr_ m str 0 str

-- -- Works, but hard to use
-- matchStr_ :: Matcher -> [Word8] -> BS.ByteString -> MatchResult
-- matchStr_ m !out0 !str =
--   case BS.uncons str of
--     Nothing -> NoMatch m
--     Just (h, t) ->
--       case M.bufferedMatcherStep m h of
--         M.StepMatch out m' -> Match m' (BS.pack out) t
--         --                                       TODO: oh oh, very bad
--         M.StepNoMatch out m' -> matchStr_ m' (out0 <> out) t

-- matchStr :: Matcher -> ByteString -> MatchResult
-- matchStr m str = matchStr_ m [] str
