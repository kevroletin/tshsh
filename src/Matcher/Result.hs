{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Matcher.Result
  ( StepResult (..),
    _StepMatch,
    _StepNoMatch,
    MatchResult (..),
    _Match,
    _NoMatch,
    mapMatcher,
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

mapMatcher :: (m -> m') -> MatchResult m arr -> MatchResult m' arr
mapMatcher f m = m { _match_matcher = f x } where x = _match_matcher m

$(makePrisms 'NoMatch)
