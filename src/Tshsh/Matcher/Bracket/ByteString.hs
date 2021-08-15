{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tshsh.Matcher.Bracket.ByteString
  ( mkMatcher,
    matcherStep,
    matchStr,
    Matcher,
    MatchResult,
  )
where

import qualified Tshsh.Matcher.Bracket.Base as B
import qualified Tshsh.Matcher.Result as R
import Protolude

{-# SPECIALIZE B.matcherStep :: B.BracketMatcher Word8 a -> Word8 -> R.StepResult (B.BracketMatcher Word8 a) a #-}

{-# SPECIALIZE B.matchStr :: B.BracketMatcher Word8 a -> ByteString -> R.MatchResult (B.BracketMatcher Word8 a) ByteString a #-}

type Matcher a = B.BracketMatcher Word8 a

type MatchResult a = R.MatchResult (Matcher a) ByteString a

type StepResult a = R.StepResult (Matcher a) a

mkMatcher :: a -> ByteString -> ByteString -> Matcher a
mkMatcher = B.mkMatcher

matcherStep :: Matcher a -> Word8 -> StepResult a
matcherStep = B.matcherStep

matchStr :: Matcher a -> ByteString -> MatchResult a
matchStr = B.matchStr
