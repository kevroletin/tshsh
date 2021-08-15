{-# OPTIONS_GHC -fno-warn-orphans #-}

module Matcher.Seq.Text
  ( mkMatcher,
    matcherStep,
    matchStr,
    Matcher,
    MatchResult,
  )
where

import qualified Matcher.Result as R
import qualified Matcher.Seq.Base as B
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
