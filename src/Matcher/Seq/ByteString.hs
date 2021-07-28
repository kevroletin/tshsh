{-# LANGUAGE RankNTypes #-}

module Matcher.Seq.ByteString
  ( mkMatcher,
    M.matcherStep,
    matchStr,
    Matcher,
    MatchResult (..),
  )
where

import qualified Data.ByteString as BS
import Matcher.Result
import Matcher.Seq.Unboxed (mch_nextCharUnsafe)
import qualified Matcher.Seq.Unboxed as M
import Protolude

type Matcher = M.Matcher Word8

mkMatcher :: BS.ByteString -> Matcher
mkMatcher str = M.mkMatcher (BS.unpack str)

data MatchResult
  = Match
      { match_matcher :: Matcher,
        match_matchLength :: {-# UNPACK #-} !Int,
        match_prev :: BS.ByteString,
        match_rest :: BS.ByteString
      }
  | NoMatch {match_matcher :: Matcher}
  deriving (Eq, Show)

matchStr_ :: Matcher -> BS.ByteString -> Int -> BS.ByteString -> MatchResult
matchStr_ m0 orig !pos str =
  case BS.uncons str of
    Nothing -> NoMatch m0
    Just (h, t) ->
      case M.matcherStep m0 h of
        StepMatch _ m' -> Match m' (M._mch_maxPos m0) (BS.take (1 + pos) orig) t
        StepNoMatch m' ->
          if M._mch_pos m' == 0
            then -- BS.break (== c) compiles into a fast memchr call which gives a huge
            -- speedup in a case when the first letter of a pattern isn't common in
            -- the input string (for example "\n" or a beginning of an escape sequence
            -- in a output from a shell command

              let c = mch_nextCharUnsafe m'
                  (skip, rest) = BS.break (== c) t
               in matchStr_ m' orig (pos + 1 + BS.length skip) rest
            else matchStr_ m' orig (pos + 1) t

matchStr :: Matcher -> ByteString -> MatchResult
matchStr m str = matchStr_ m str 0 str
