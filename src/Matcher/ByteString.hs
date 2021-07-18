module Matcher.ByteString
  ( mkMatcher,
    M.matcherStep,
    matchStr,
    Matcher,
    MatchResult (..),
    M.StepResult (..),
  )
where

import qualified Data.ByteString as BS
import qualified Matcher.Unboxed as M
import Protolude
import Control.Lens ((^.))

type Matcher = M.Matcher Word8

mkMatcher :: BS.ByteString -> Matcher
mkMatcher str = M.mkMatcher (BS.unpack str)

data MatchResult = Match { match_matcher :: Matcher,
                           match_prev :: BS.ByteString,
                           match_rest :: BS.ByteString }
                 | NoMatch { match_matcher :: Matcher }
                 deriving (Eq, Show)

matchStr_ :: Matcher -> BS.ByteString -> Int -> BS.ByteString -> MatchResult
matchStr_ m orig pos str =
  case BS.uncons str of
    Nothing -> NoMatch m
    Just (h, t) ->
      case M.matcherStep m h of
        M.StepMatch m'   -> Match m' (BS.take (1 + pos - m ^. M.mch_maxPos) orig) t
        M.StepNoMatch m' -> matchStr_ m' orig (pos + 1) t

matchStr :: Matcher -> ByteString -> MatchResult
matchStr m str = matchStr_ m str 0 str
