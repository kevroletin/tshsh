{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tshsh.Matcher.Seq.Text
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

{-# SPECIALIZE INLINE B.matcherStep :: B.SeqMatcher Char a -> Char -> R.StepResult (B.SeqMatcher Char a) a #-}

{-# SPECIALIZE B.matchStr :: B.SeqMatcher Char a -> Text -> R.MatchResult (B.SeqMatcher Char a) Text a #-}

type Matcher a = B.SeqMatcher Char a

type MatchResult a = R.MatchResult (Matcher a) Text a

type StepResult a = R.StepResult (Matcher a) a

mkMatcher :: a -> Text -> Matcher a
mkMatcher = B.mkMatcher

matchStr :: Matcher a -> Text -> MatchResult a
matchStr = B.matchStr

matcherStep :: Matcher a -> Char -> StepResult a
matcherStep = B.matcherStep

matcherReset :: Matcher a -> Matcher a
matcherReset = B.matcherReset
