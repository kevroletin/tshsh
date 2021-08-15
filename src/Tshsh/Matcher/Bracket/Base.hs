{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Matcher.Bracket.Base
  ( BracketMatcher (..),
    bmch_left,
    bmch_right,
    bmch_leftMatchOffset,
    bmch_ret,
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
import Tshsh.Matcher.Result
import qualified Tshsh.Matcher.Seq.Base as SeqM
import Protolude

data BracketMatcher c a = BracketMatcher
  { _bmch_left :: SeqM.SeqMatcher c (),
    _bmch_right :: SeqM.SeqMatcher c (),
    _bmch_leftMatchOffset :: {-# UNPACK #-} !Int,
    _bmch_ret :: a
  }

$(makeLenses 'BracketMatcher)

deriving instance Eq a => Eq (BracketMatcher Char a)

deriving instance Show a => Show (BracketMatcher Char a)

deriving instance Eq a => Eq (BracketMatcher Word8 a)

deriving instance Show a => Show (BracketMatcher Word8 a)

type CanUnbox c = U.IArray U.UArray c

mkMatcher' :: (Eq c, CanUnbox c) => a -> [c] -> [c] -> BracketMatcher c a
mkMatcher' ret left right =
  BracketMatcher
    { _bmch_left = SeqM.mkMatcher () left,
      _bmch_right = SeqM.mkMatcher () right,
      _bmch_leftMatchOffset = -1,
      _bmch_ret = ret
    }

mkMatcher :: (Eq c, CanUnbox c, ListLike list c) => a -> list -> list -> BracketMatcher c a
mkMatcher ret l r = mkMatcher' ret (L.toList l) (L.toList r)

matcherReset :: BracketMatcher c a -> BracketMatcher c a
matcherReset BracketMatcher {..} =
  BracketMatcher
    { _bmch_left = SeqM.matcherReset _bmch_left,
      _bmch_right = SeqM.matcherReset _bmch_right,
      _bmch_leftMatchOffset = -1,
      _bmch_ret = _bmch_ret
    }
{-# INLINE matcherReset #-}

matcherStep :: (Eq c, CanUnbox c) => BracketMatcher c a -> c -> StepResult (BracketMatcher c a) a
matcherStep m0 c =
  let offset = m0 ^. bmch_leftMatchOffset
   in if offset < 0
        then StepNoMatch $
          case SeqM.matcherStep (m0 ^. bmch_left) c of
            StepMatch _ lm' () ->
              m0 & bmch_left .~ lm'
                & bmch_leftMatchOffset .~ (lm' ^. SeqM.mch_maxPos)
                & bmch_right %~ SeqM.matcherReset
            StepNoMatch lm' ->
              m0 & bmch_left .~ lm'
        else case SeqM.matcherStep (m0 ^. bmch_right) c of
          StepMatch _ _ () -> StepMatch (offset + 1) (matcherReset m0) (_bmch_ret m0)
          StepNoMatch mr' ->
            StepNoMatch
              ( m0 & bmch_right .~ mr'
                  & bmch_leftMatchOffset %~ (+ 1)
              )
{-# INLINEABLE matcherStep #-}

matchStr ::
  (Eq c, CanUnbox c, ListLike full c) =>
  BracketMatcher c a ->
  full ->
  MatchResult (BracketMatcher c a) full a
matchStr m0 str0 =
  if _bmch_leftMatchOffset m0 < 0
    then matchLeft
    else matchRight 0 m0 str0
  where
    matchLeft =
      case SeqM.matchStr (_bmch_left m0) str0 of
        Match m len prev rest () ->
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
      case SeqM.matchStr (_bmch_right m1) str of
        Match _ _ prev rest () ->
          Match
            (matcherReset m1)
            (_bmch_leftMatchOffset m1 + L.length prev)
            (L.take (pos + L.length prev) str0)
            rest
            (_bmch_ret m1)
        NoMatch m ->
          NoMatch
            ( m1
                { _bmch_right = m,
                  _bmch_leftMatchOffset = _bmch_leftMatchOffset m1 + L.length str
                }
            )
{-# INLINEABLE matchStr #-}
