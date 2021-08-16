{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Tshsh.Matcher.Bracket
  ( BracketMatcher (..),
    bmch_left,
    bmch_right,
    bmch_leftMatchOffset,
    mkBracketMatcher,
    matcherReset,
    matchStr,
    mkBracketMatcherSC,
  )
where

import Control.Lens
import qualified Data.Array.Unboxed as U
import Data.ListLike (ListLike)
import qualified Data.ListLike as L
import qualified Tshsh.Matcher.Seq as SeqM
import Protolude
import Tshsh.Stream
import Data.Coerce
import GHC.Exts (IsList(Item))

data BracketMatcher list = BracketMatcher
  { _bmch_left :: SeqM.SeqMatcher list,
    _bmch_right :: SeqM.SeqMatcher list,
    _bmch_leftMatchOffset :: {-# UNPACK #-} !Int
  }

$(makeLenses 'BracketMatcher)

type CanUnbox c = U.IArray U.UArray c

deriving instance (c ~ Item list, Eq c, CanUnbox c) => Eq (BracketMatcher list)

deriving instance (c ~ Item list, Show c, CanUnbox c) => Show (BracketMatcher list)

mkBracketMatcher :: (ListLike list c, Eq c, CanUnbox c) => list -> list -> BracketMatcher list
mkBracketMatcher left right =
  BracketMatcher
    { _bmch_left = SeqM.mkSeqMatcher left,
      _bmch_right = SeqM.mkSeqMatcher right,
      _bmch_leftMatchOffset = -1
    }
{-# INLINE mkBracketMatcher #-}

matcherReset :: BracketMatcher c -> BracketMatcher c
matcherReset BracketMatcher {..} =
  BracketMatcher
    { _bmch_left = SeqM.matcherReset _bmch_left,
      _bmch_right = SeqM.matcherReset _bmch_right,
      _bmch_leftMatchOffset = -1
    }
{-# INLINE matcherReset #-}

matchStr ::
  (Eq c, CanUnbox c, ListLike list c) =>
  BracketMatcher list ->
  list ->
  ConsumerResult (BracketMatcher list) list Int
matchStr m0 str0 =
  if _bmch_leftMatchOffset m0 < 0
    then matchLeft
    else matchRight 0 m0 str0
  where
    matchLeft =
      case SeqM.matchStr (_bmch_left m0) str0 of
        ConsumerFinish m prev rest len ->
          matchRight
            (L.length prev)
            ( m0
                { _bmch_left = m,
                  _bmch_leftMatchOffset = len
                }
            )
            rest
        ConsumerContinue m ->
          ConsumerContinue (m0 {_bmch_left = m})
    matchRight pos m1 str =
      case SeqM.matchStr (_bmch_right m1) str of
        ConsumerFinish _ prev rest _ ->
          ConsumerFinish
            (matcherReset m1)
            (L.take (pos + L.length prev) str0)
            rest
            (_bmch_leftMatchOffset m1 + L.length prev)
        ConsumerContinue m ->
          ConsumerContinue
            ( m1
                { _bmch_right = m,
                  _bmch_leftMatchOffset = _bmch_leftMatchOffset m1 + L.length str
                }
            )
{-# INLINEABLE matchStr #-}

newtype Wrapper a b c = Wrapper (a b)
  deriving Show

instance (ListLike list item, Eq item, CanUnbox item) => ConsumerI (Wrapper BracketMatcher) list Int where
  consumeI m str = coerce matchStr m str

  resetI = coerce matcherReset

mkBracketMatcherSC ::
  (ListLike list item, Eq item, Show item, CanUnbox item) =>
  list ->
  list ->
  StreamConsumer list Int
mkBracketMatcherSC l r = StreamConsumer . Wrapper $ mkBracketMatcher l r
{-# INLINE mkBracketMatcherSC #-}
