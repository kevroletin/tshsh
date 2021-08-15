{-# OPTIONS_GHC -fno-warn-orphans #-}

module Matcher.Bracket.Text
  ( mkMatcher,
    matcherStep,
    matchStr,
    Matcher,
    MatchResult,
  )
where

import qualified Matcher.Bracket.Base as B
import qualified Matcher.Result as R
import Protolude

{-# SPECIALIZE INLINE B.matcherStep :: B.BracketMatcher Char a -> Char -> R.StepResult (B.BracketMatcher Char a) a #-}

{-# SPECIALIZE B.matchStr :: B.BracketMatcher Char a -> Text -> R.MatchResult (B.BracketMatcher Char a) Text a #-}

type Matcher a = B.BracketMatcher Char a

type MatchResult a = R.MatchResult (Matcher a) Text a

type StepResult a = R.StepResult (Matcher a) a

mkMatcher :: a -> Text -> Text -> Matcher a
mkMatcher = B.mkMatcher

matcherStep :: Matcher a -> Char -> StepResult a
matcherStep = B.matcherStep

matchStr :: Matcher a -> Text -> MatchResult a
matchStr = B.matchStr
