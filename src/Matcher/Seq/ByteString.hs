module Matcher.Seq.ByteString
  ( mkMatcher,
    U.matcherStep,
    matchStr,
    Matcher,
    MatchResult,
  )
where

import qualified Matcher.Result as R
import qualified Matcher.Seq.Unboxed as U
import Protolude

{-# SPECIALIZE INLINE U.matcherStep :: U.Matcher Word8 -> Word8 -> R.StepResult (U.Matcher Word8) #-}

{-# SPECIALIZE U.matchStr :: U.Matcher Word8 -> ByteString -> R.MatchResult (U.Matcher Word8) ByteString #-}

type Matcher = U.Matcher Word8

type MatchResult = R.MatchResult Matcher ByteString

type StepResult = R.StepResult Matcher

mkMatcher :: ByteString -> Matcher
mkMatcher = U.mkMatcher

matchStr :: Matcher -> ByteString -> MatchResult
matchStr = U.matchStr

matcherStep :: Matcher -> Word8 -> StepResult
matcherStep = U.matcherStep
