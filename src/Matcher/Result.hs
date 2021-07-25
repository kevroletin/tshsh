{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Matcher.Result
  ( StepResult (..),
    _StepMatch,
    _StepNoMatch,
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
  deriving (Functor)

$(makePrisms 'StepNoMatch)
