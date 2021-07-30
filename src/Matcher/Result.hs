{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Matcher.Result
  ( StepResult (..),
    _StepMatch,
    match_matcher,
    match_matchLength,
    match_prev,
    match_rest,
    _StepNoMatch,
    mapStepResult,
    MatchResult (..),
    _Match,
    _NoMatch,
    mapMatchResult,
  )
where

import Control.Lens
import Protolude

data StepResult m
  = StepMatch
      { _sr_matchLength :: {-# UNPACK #-} !Int,
        _sr_matcher :: m
      }
  | StepNoMatch {_sr_matcher :: m}
  deriving (Show)

mapStepResult :: (m -> m') -> StepResult m -> StepResult m'
mapStepResult f m = m { _sr_matcher = f x } where x = _sr_matcher m

$(makePrisms 'StepNoMatch)

data MatchResult m arr
  = Match
      { _match_matcher :: m,
        _match_matchLength :: {-# UNPACK #-} !Int,
        _match_prev :: arr,
        _match_rest :: arr
      }
  | NoMatch {_match_matcher :: m}
  deriving Show

mapMatchResult :: (m -> m') -> MatchResult m arr -> MatchResult m' arr
mapMatchResult f m = m { _match_matcher = f x } where x = _match_matcher m

$(makeLenses 'Match)
$(makePrisms 'NoMatch)
