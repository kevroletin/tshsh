{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Matcher.Result
  ( StepResult (..),
    _StepMatch,
    match_matcher,
    match_matchLength,
    match_prev,
    match_rest,
    match_result,
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

data StepResult m a
  = StepMatch
      { _sr_matchLength :: {-# UNPACK #-} !Int,
        _sr_matcher :: m,
        _sr_result :: a
      }
  | StepNoMatch { _sr_matcher :: m }
  deriving (Show)

mapStepResult :: (m -> m') -> StepResult m a -> StepResult m' a
mapStepResult f m = m { _sr_matcher = f x } where x = _sr_matcher m

$(makePrisms 'StepNoMatch)

data MatchResult m arr a
  = Match
      { _match_matcher :: m,
        _match_matchLength :: {-# UNPACK #-} !Int,
        _match_prev :: arr,
        _match_rest :: arr,
        _match_result :: a
      }
  | NoMatch {_match_matcher :: m}
  deriving Show

mapMatchResult :: (m -> m') -> MatchResult m arr a -> MatchResult m' arr a
mapMatchResult f m = m { _match_matcher = f x } where x = _match_matcher m

$(makeLenses 'Match)
$(makePrisms 'NoMatch)
