module Matcher.ByteString
  ( SomeMatcher,
    MatchResult,
    matchStr,
    matcherStep,
    mkSeqMatcher,
    mkBracketMatcher,
    matcherReset
  )
where

import qualified Matcher.Result as R
import qualified Matcher.Unboxed as U
import Protolude

type SomeMatcher = U.SomeMatcher ByteString Word8

type MatchResult = R.MatchResult SomeMatcher ByteString

type StepResult = R.StepResult SomeMatcher

matcherReset :: SomeMatcher -> SomeMatcher
matcherReset = U.matcherReset
{-# INLINE matcherReset #-}

matcherStep :: SomeMatcher -> Word8 -> StepResult
matcherStep = U.matcherStep
{-# INLINE matcherStep #-}

matchStr :: SomeMatcher -> ByteString -> MatchResult
matchStr = U.matchStr
{-# INLINE matchStr #-}

mkSeqMatcher :: ByteString -> SomeMatcher
mkSeqMatcher = U.mkSeqMatcher
{-# INLINE mkSeqMatcher #-}

mkBracketMatcher :: ByteString -> ByteString -> SomeMatcher
mkBracketMatcher = U.mkBracketMatcher
{-# INLINE mkBracketMatcher #-}
