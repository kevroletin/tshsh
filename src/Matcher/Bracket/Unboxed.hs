{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Matcher.Bracket.Unboxed
  ( Matcher (..),
    bmch_left,
    bmch_right,
    bmch_leftMatchOffset,
    mkMatcher,
    matcherReset,
    matcherStep,
  )
where

import Control.Lens
import qualified Data.Array.Unboxed as U
import Matcher.Result
import qualified Matcher.Seq.Unboxed as M
import Protolude

data Matcher a = Matcher
  { _bmch_left :: M.Matcher a,
    _bmch_right :: M.Matcher a,
    _bmch_leftMatchOffset :: Maybe Int
  }

$(makeLenses 'Matcher)

deriving instance Eq (Matcher Char)

deriving instance Show (Matcher Char)

deriving instance Eq (Matcher Word8)

deriving instance Show (Matcher Word8)

type CanUnbox a = U.IArray U.UArray a

mkMatcher :: forall a. (Eq a, CanUnbox a) => [a] -> [a] -> Matcher a
mkMatcher left right =
  Matcher
    { _bmch_left = M.mkMatcher left,
      _bmch_right = M.mkMatcher right,
      _bmch_leftMatchOffset = Nothing
    }

matcherReset :: Matcher a -> Matcher a
matcherReset Matcher {..} =
  Matcher
    { _bmch_left = M.matcherReset _bmch_left,
      _bmch_right = M.matcherReset _bmch_right,
      _bmch_leftMatchOffset = Nothing
    }

matcherStep :: (Eq a, CanUnbox a) => Matcher a -> a -> StepResult (Matcher a)
matcherStep m0 a =
  case m0 ^. bmch_leftMatchOffset of
    Nothing ->
      StepNoMatch $
        case M.matcherStep (m0 ^. bmch_left) a of
          StepMatch _ lm' ->
            m0 & bmch_left .~ lm'
              & bmch_leftMatchOffset ?~ (lm' ^. M.mch_maxPos)
              & bmch_right %~ M.matcherReset
          StepNoMatch lm' ->
            m0 & bmch_left .~ lm'
    Just offset ->
      case M.matcherStep (m0 ^. bmch_right) a of
        StepMatch _ _ -> StepMatch (offset + 1) (matcherReset m0)
        StepNoMatch mr' ->
          StepNoMatch
            ( m0 & bmch_right .~ mr'
                & bmch_leftMatchOffset . _Just %~ (+ 1)
            )
