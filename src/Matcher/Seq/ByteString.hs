{-# LANGUAGE RankNTypes #-}

module Matcher.Seq.ByteString
  ( mkMatcher,
    M.matcherStep,
    matchStr,
    Matcher,
    MatchResult (..),
  )
where

import Control.Lens (ix, (.~), (^.), (^?))
import qualified Data.ByteString as BS
import Matcher.Seq.Unboxed (matcherReset, mch_forwardUnsafe, mch_isFull, mch_jumpTable, mch_nextCharUnsafe, mch_pos)
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
    Just (inpChr, t) ->
      if mch_nextCharUnsafe m0 == inpChr
        then
          let m = mch_forwardUnsafe m0
           in if mch_isFull m
                then Match (matcherReset m) (m ^. M.mch_maxPos) (BS.take (1 + pos) orig) t
                else matchStr_ m orig (pos + 1) t
        else
          if m0 ^. mch_pos == 0
            then
              let c = mch_nextCharUnsafe m0
                  (skip, rest) = BS.break (== c) t
               in matchStr_ m0 orig (pos + 1 + BS.length skip) rest
            else
              let Just fallbackPos = m0 ^? mch_jumpTable . ix (m0 ^. mch_pos)
               in matchStr_ (m0 & mch_pos .~ fallbackPos) orig pos str

matchStr :: Matcher -> ByteString -> MatchResult
matchStr m str = matchStr_ m str 0 str
