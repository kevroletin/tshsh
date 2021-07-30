module Matcher.Bracket.ByteString
  ( mkMatcher,
    matcherStep,
    matchStr,
    Matcher,
    MatchResult,
  )
where

import qualified Matcher.Bracket.Unboxed as U
import qualified Matcher.Result as R
import qualified Matcher.Seq.ByteString as S
import Protolude

{-# SPECIALIZE U.matcherStep :: U.Matcher Word8 -> Word8 -> R.StepResult (U.Matcher Word8) #-}

{-# SPECIALIZE U.matchStr :: U.Matcher Word8 -> ByteString -> R.MatchResult (U.Matcher Word8) ByteString #-}

type Matcher = U.Matcher Word8

type MatchResult = R.MatchResult Matcher ByteString

type StepResult = R.StepResult Matcher

mkMatcher :: ByteString -> ByteString -> U.Matcher Word8
mkMatcher = U.mkMatcher

matcherStep :: Matcher -> Word8 -> StepResult
matcherStep = U.matcherStep

matchStr :: Matcher -> ByteString -> MatchResult
matchStr = U.matchStr
