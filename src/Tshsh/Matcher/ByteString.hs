module Tshsh.Matcher.ByteString
  ( SomeMatcher,
    MatchResult,
    matchStr,
    matcherStep,
    mkSeqMatcher,
    mkBracketMatcher,
    matcherReset
  )
where

import qualified Tshsh.Matcher.Result as R
import qualified Tshsh.Matcher.Base as B
import Protolude

type SomeMatcher = B.SomeMatcher ByteString Word8

type MatchResult a = R.MatchResult (SomeMatcher a) ByteString a

type StepResult a = R.StepResult (SomeMatcher a) a

matcherReset :: SomeMatcher a -> SomeMatcher a
matcherReset = B.matcherReset
{-# INLINE matcherReset #-}

matcherStep :: SomeMatcher a -> Word8 -> StepResult a
matcherStep = B.matcherStep
{-# INLINE matcherStep #-}

matchStr :: SomeMatcher a -> ByteString -> MatchResult a
matchStr = B.matchStr
{-# INLINE matchStr #-}

mkSeqMatcher :: Show a => a -> ByteString -> SomeMatcher a
mkSeqMatcher = B.mkSeqMatcher
{-# INLINE mkSeqMatcher #-}

mkBracketMatcher :: Show a =>  a -> ByteString -> ByteString -> SomeMatcher a
mkBracketMatcher = B.mkBracketMatcher
{-# INLINE mkBracketMatcher #-}
