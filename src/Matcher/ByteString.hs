module Matcher.ByteString
  ( Matcher,
    MatchResult (..),
    matchStr,
    mkSeqMatcher,
    mkBracketMatcher,
  )
where

import qualified Data.ByteString as BS
import Matcher.Result
import qualified Matcher.Unboxed as M
import Protolude
import Prelude (Show (..))

type Matcher = M.Matcher Word8

data MatchResult
  = Match
      { match_matcher :: Matcher,
        match_matchLength :: {-# UNPACK #-} !Int,
        match_prev :: BS.ByteString,
        match_rest :: BS.ByteString
      }
  | NoMatch {match_matcher :: Matcher}

instance Show MatchResult where
  show (Match _ l p r) =
    "(Match _ " <> Protolude.show l <> " " <> Protolude.show p <> " " <> Protolude.show r <> ")"
  show (NoMatch _) = "NoMatch"

matchStr_ :: M.MatcherI m Word8 => m Word8 -> BS.ByteString -> Int -> BS.ByteString -> MatchResult
matchStr_ m orig !pos str =
  case BS.uncons str of
    Nothing -> NoMatch (M.Matcher m)
    Just (h, t) ->
      case M.matcherStep m h of
        StepMatch len m' -> Match (M.Matcher m') len (BS.take (1 + pos) orig) t
        StepNoMatch m' -> matchStr_ m' orig (pos + 1) t

matchStr :: Matcher -> ByteString -> MatchResult
matchStr m str = M.applyMatcher m (\m' -> matchStr_ m' str 0 str)

mkSeqMatcher :: ByteString -> Matcher
mkSeqMatcher = M.mkSeqMatcher . BS.unpack

mkBracketMatcher :: ByteString -> ByteString -> Matcher
mkBracketMatcher l r = M.mkBracketMatcher (BS.unpack l) (BS.unpack r)
