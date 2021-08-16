module Tshsh.Matcher
  ( mkSeqMatcher,
    mkBracketMatcher,
    module S,
  )
where

import Data.Coerce
import qualified Tshsh.Matcher.Bracket as BrM
import qualified Tshsh.Matcher.Seq as SeqM
import qualified Tshsh.Stream as S
import Tshsh.Stream
import Protolude

newtype SeqMatcherWrapper b c = SeqMatcherWrapper SeqM.SeqMatcher
  deriving (Show)

instance ConsumerI SeqMatcherWrapper ByteString Int where
  consumeI = coerce SeqM.matchStr
  resetI = coerce SeqM.matcherReset

mkSeqMatcher ::
  ByteString ->
  StreamConsumer ByteString Int
mkSeqMatcher = StreamConsumer . SeqMatcherWrapper . SeqM.mkSeqMatcher
{-# INLINE mkSeqMatcher #-}

newtype BracketMatcherWrapper b c = BracketMatcherWrapper BrM.BracketMatcher
  deriving (Show)

instance ConsumerI BracketMatcherWrapper ByteString Int where
  consumeI = coerce BrM.matchStr
  resetI = coerce BrM.matcherReset

mkBracketMatcher ::
  ByteString ->
  ByteString ->
  StreamConsumer ByteString Int
mkBracketMatcher l r = StreamConsumer . BracketMatcherWrapper $ BrM.mkBracketMatcher l r
{-# INLINE mkBracketMatcher #-}
