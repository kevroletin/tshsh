{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Matcher.Unboxed
  ( Matcher (..),
    matcherStep,
    matcherReset,
    mkSeqMatcher,
    mkBracketMatcher,
  )
where

import qualified Data.Array.Unboxed as U
import qualified Matcher.Bracket.Unboxed as MB
import Matcher.Result
import qualified Matcher.Seq.Unboxed as MS
import Protolude

type CanUnbox a = U.IArray U.UArray a

data Matcher a = forall m.
  Matcher
  { _mch_self :: m a,
    _mch_step :: (Eq a, CanUnbox a) => m a -> a -> StepResult (m a),
    _mch_reset :: m a -> m a
  }

matcherStep :: (Eq a, CanUnbox a) => Matcher a -> a -> StepResult (Matcher a)
matcherStep Matcher {..} a =
  (\m' -> Matcher m' _mch_step _mch_reset) <$> _mch_step _mch_self a

matcherReset :: Matcher a -> Matcher a
matcherReset Matcher {..} =
  Matcher (_mch_reset _mch_self) _mch_step _mch_reset

mkSeqMatcher :: (Eq a, CanUnbox a) => [a] -> Matcher a
mkSeqMatcher xs =
  Matcher
    { _mch_self = MS.mkMatcher xs,
      _mch_step = MS.matcherStep,
      _mch_reset = MS.matcherReset
    }

mkBracketMatcher :: (Eq a, CanUnbox a) => [a] -> [a] -> Matcher a
mkBracketMatcher left right =
  Matcher
    { _mch_self = MB.mkMatcher left right,
      _mch_step = MB.matcherStep,
      _mch_reset = MB.matcherReset
    }
