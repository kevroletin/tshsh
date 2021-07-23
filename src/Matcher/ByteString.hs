{-# LANGUAGE StrictData #-}

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
        match_matchLength :: Int,
        match_prev :: BS.ByteString,
        match_rest :: BS.ByteString
      }
  | NoMatch {match_matcher :: Matcher}

instance Show MatchResult where
  show (Match _ l p r) =
    "(Match _ " <> Protolude.show l <> " " <> Protolude.show p <> " " <> Protolude.show r <> ")"
  show (NoMatch _) = "NoMatch"

matchStr_ :: Matcher -> BS.ByteString -> Int -> BS.ByteString -> MatchResult
matchStr_ (M.Matcher m0 step reset) = go m0
  where
    pack m = M.Matcher m step reset
    go m orig pos str =
      case BS.uncons str of
        Nothing -> NoMatch (pack m)
        Just (h, t) ->
          case step m h of
            StepMatch len m' -> Match (pack m') len (BS.take (1 + pos) orig) t
            StepNoMatch m' -> go m' orig (pos + 1) t

matchStr :: Matcher -> ByteString -> MatchResult
matchStr m str = matchStr_ m str 0 str

mkSeqMatcher :: ByteString -> Matcher
mkSeqMatcher = M.mkSeqMatcher . BS.unpack

mkBracketMatcher :: ByteString -> ByteString -> Matcher
mkBracketMatcher l r = M.mkBracketMatcher (BS.unpack l) (BS.unpack r)
