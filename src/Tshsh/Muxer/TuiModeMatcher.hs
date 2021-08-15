{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Muxer.TuiModeMatcher (tuiModeMatcher) where

import qualified Data.ByteString as BS
import qualified Tshsh.Matcher.Base as B
import Tshsh.Matcher.Result
import qualified Tshsh.Matcher.Seq.ByteString as SeqMBs
import Protolude

data TuiModeMatcher c a = TuiModeMatcher
  { _tmch_prefixMatcher :: SeqMBs.Matcher (),
    _tmch_prefixMatchOffset :: {-# UNPACK #-} Int
  }
  deriving (Show)

instance B.MatcherI TuiModeMatcher Word8 Bool where
  matcherStepI = panic "not implemented"
  matcherResetI = matcherReset

instance B.MatcherArrI TuiModeMatcher ByteString Word8 Bool where
  matchStrI = matchStr

tuiModeMatcher :: B.SomeMatcher ByteString Word8 Bool
tuiModeMatcher = B.SomeMatcher mkMatcher

mkMatcher :: TuiModeMatcher c a
mkMatcher =
  TuiModeMatcher
    { _tmch_prefixMatcher = SeqMBs.mkMatcher () "\ESC[?1049",
      _tmch_prefixMatchOffset = -1
    }

matchStr ::
  TuiModeMatcher c a ->
  ByteString ->
  MatchResult (TuiModeMatcher c a) ByteString Bool
matchStr m0 str0 =
  if _tmch_prefixMatchOffset m0 < 0
    then matchPrefix
    else matchParam m0 str0
  where
    matchPrefix =
      case SeqMBs.matchStr (_tmch_prefixMatcher m0) str0 of
        Match m len _prev rest () ->
          matchParam
            ( m0
                { _tmch_prefixMatcher = m,
                  _tmch_prefixMatchOffset = len
                }
            )
            rest
        NoMatch m ->
          NoMatch (m0 {_tmch_prefixMatcher = m})
    matchParam m1 str =
      case BS.uncons str of
        Nothing -> NoMatch m1
        Just (x, xs) ->
          if x == 104 || x == 108
            then
              Match
                (matcherReset m1)
                (_tmch_prefixMatchOffset m1 + 1)
                (BS.take (BS.length str0 - BS.length xs) str0)
                xs
                (x == 104)
            else NoMatch (matcherReset m1)

matcherReset :: TuiModeMatcher c a -> TuiModeMatcher c a
matcherReset TuiModeMatcher {..} =
  TuiModeMatcher
    { _tmch_prefixMatcher = SeqMBs.matcherReset _tmch_prefixMatcher,
      _tmch_prefixMatchOffset = -1
    }
