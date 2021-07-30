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
    matchStr,
  )
where

import Control.Lens
import qualified Data.Array.Unboxed as U
import Data.ListLike (ListLike)
import qualified Data.ListLike as L
import Matcher.Result
import qualified Matcher.Seq.Unboxed as M
import Protolude

data Matcher a = Matcher
  { _bmch_left :: M.Matcher a,
    _bmch_right :: M.Matcher a,
    _bmch_leftMatchOffset :: {-# UNPACK #-} !Int
  }

$(makeLenses 'Matcher)

deriving instance Eq (Matcher Char)

deriving instance Show (Matcher Char)

deriving instance Eq (Matcher Word8)

deriving instance Show (Matcher Word8)

type CanUnbox a = U.IArray U.UArray a

mkMatcher' :: forall a. (Eq a, CanUnbox a) => [a] -> [a] -> Matcher a
mkMatcher' left right =
  Matcher
    { _bmch_left = M.mkMatcher left,
      _bmch_right = M.mkMatcher right,
      _bmch_leftMatchOffset = -1
    }

mkMatcher :: (Eq a, CanUnbox a, ListLike list a) => list -> list -> Matcher a
mkMatcher l r = mkMatcher' (L.toList l) (L.toList r)

matcherReset :: Matcher a -> Matcher a
matcherReset Matcher {..} =
  Matcher
    { _bmch_left = M.matcherReset _bmch_left,
      _bmch_right = M.matcherReset _bmch_right,
      _bmch_leftMatchOffset = -1
    }
{-# INLINE matcherReset #-}

matcherStep :: (Eq a, CanUnbox a) => Matcher a -> a -> StepResult (Matcher a)
matcherStep m0 a =
  let offset = m0 ^. bmch_leftMatchOffset
   in if offset < 0
        then StepNoMatch $
          case M.matcherStep (m0 ^. bmch_left) a of
            StepMatch _ lm' ->
              m0 & bmch_left .~ lm'
                & bmch_leftMatchOffset .~ (lm' ^. M.mch_maxPos)
                & bmch_right %~ M.matcherReset
            StepNoMatch lm' ->
              m0 & bmch_left .~ lm'
        else case M.matcherStep (m0 ^. bmch_right) a of
          StepMatch _ _ -> StepMatch (offset + 1) (matcherReset m0)
          StepNoMatch mr' ->
            StepNoMatch
              ( m0 & bmch_right .~ mr'
                  & bmch_leftMatchOffset %~ (+ 1)
              )
{-# INLINEABLE matcherStep #-}

matchStr ::
  (Eq a, CanUnbox a, ListLike full a) =>
  Matcher a ->
  full ->
  MatchResult (Matcher a) full
matchStr m0 str0 =
  if _bmch_leftMatchOffset m0 < 0
    then matchLeft
    else matchRight 0 m0 str0
  where
    matchLeft =
      case M.matchStr (_bmch_left m0) str0 of
        Match m len prev rest ->
          matchRight
            (L.length prev)
            ( m0
                { _bmch_left = m,
                  _bmch_leftMatchOffset = len
                }
            )
            rest
        NoMatch m ->
          NoMatch (m0 {_bmch_left = m})
    matchRight pos m1 str =
      case M.matchStr (_bmch_right m1) str of
        Match _ _ prev rest ->
          Match
            (matcherReset m1)
            (_bmch_leftMatchOffset m1 + L.length prev)
            (L.take (pos + L.length prev) str0)
            rest
        NoMatch m ->
          NoMatch
            ( m1
                { _bmch_right = m,
                  _bmch_leftMatchOffset = _bmch_leftMatchOffset m1 + L.length str
                }
            )
{-# INLINEABLE matchStr #-}
