{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tshsh.Matcher.Seq.ByteString
  ( mkMatcher,
    matcherStep,
    matchStr,
    matcherReset,
    Matcher,
    MatchResult,
  )
where

import qualified Tshsh.Matcher.Result as R
import qualified Tshsh.Matcher.Seq.Base as B
import Protolude

{-# SPECIALIZE INLINE B.matcherStep :: B.SeqMatcher Word8 a -> Word8 -> R.StepResult (B.SeqMatcher Word8 a) a #-}

{-# SPECIALIZE B.matchStr :: B.SeqMatcher Word8 a -> ByteString -> R.MatchResult (B.SeqMatcher Word8 a) ByteString a #-}

type Matcher a = B.SeqMatcher Word8 a

type MatchResult a = R.MatchResult (Matcher a) ByteString a

type StepResult a = R.StepResult (Matcher a) a

mkMatcher :: a -> ByteString -> Matcher a
mkMatcher = B.mkMatcher

matchStr :: Matcher a -> ByteString -> MatchResult a
matchStr = B.matchStr

matcherStep :: Matcher a -> Word8 -> StepResult a
matcherStep = B.matcherStep

matcherReset :: Matcher a -> Matcher a
matcherReset = B.matcherReset
