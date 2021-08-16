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
  )
where

import Control.Lens
import qualified Data.ByteString as BS
import qualified Tshsh.Matcher.Seq as SeqM
import Protolude
import Tshsh.Stream

data BracketMatcher = BracketMatcher
  { _bmch_left :: SeqM.SeqMatcher,
    _bmch_right :: SeqM.SeqMatcher,
    _bmch_leftMatchOffset :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

$(makeLenses 'BracketMatcher)

mkBracketMatcher :: ByteString -> ByteString -> BracketMatcher
mkBracketMatcher left right =
  BracketMatcher
    { _bmch_left = SeqM.mkSeqMatcher left,
      _bmch_right = SeqM.mkSeqMatcher right,
      _bmch_leftMatchOffset = -1
    }
{-# INLINE mkBracketMatcher #-}

matcherReset :: BracketMatcher -> BracketMatcher
matcherReset BracketMatcher {..} =
  BracketMatcher
    { _bmch_left = SeqM.matcherReset _bmch_left,
      _bmch_right = SeqM.matcherReset _bmch_right,
      _bmch_leftMatchOffset = -1
    }
{-# INLINE matcherReset #-}

matchStr ::
  BracketMatcher ->
  ByteString ->
  ConsumerResult BracketMatcher ByteString Int
matchStr m0 str0 =
  if _bmch_leftMatchOffset m0 < 0
    then matchLeft
    else matchRight 0 m0 str0
  where
    matchLeft =
      case SeqM.matchStr (_bmch_left m0) str0 of
        ConsumerFinish m prev rest len ->
          matchRight
            (BS.length prev)
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
            (BS.take (pos + BS.length prev) str0)
            rest
            (_bmch_leftMatchOffset m1 + BS.length prev)
        ConsumerContinue m ->
          ConsumerContinue
            ( m1
                { _bmch_right = m,
                  _bmch_leftMatchOffset = _bmch_leftMatchOffset m1 + BS.length str
                }
            )
{-# INLINEABLE matchStr #-}

