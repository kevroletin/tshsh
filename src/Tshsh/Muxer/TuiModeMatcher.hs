{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Muxer.TuiModeMatcher (tuiModeMatcher) where

import qualified Data.ByteString as BS
import Data.Coerce
import Protolude
import qualified Tshsh.Matcher.Seq as SeqM
import Tshsh.Stream

data TuiModeMatcher = TuiModeMatcher
  { _tmch_prefixMatcher :: SeqM.SeqMatcher,
    _tmch_prefixMatchOffset :: {-# UNPACK #-} Int
  }
  deriving (Show)

instance ConsumerI Wrapper ByteString (Bool, Int) where
  consumeI = coerce matchStr
  resetI = coerce matcherReset

newtype Wrapper str a = Wrapper TuiModeMatcher
  deriving (Show)

tuiModeMatcher :: StreamConsumer ByteString (Bool, Int)
tuiModeMatcher = StreamConsumer . Wrapper $ mkMatcher

mkMatcher :: TuiModeMatcher
mkMatcher =
  TuiModeMatcher
    { _tmch_prefixMatcher = SeqM.mkSeqMatcher "\ESC[?1049",
      _tmch_prefixMatchOffset = -1
    }

matchStr ::
  TuiModeMatcher ->
  ByteString ->
  ConsumerResult TuiModeMatcher ByteString (Bool, Int)
matchStr m0 str0 =
  if _tmch_prefixMatchOffset m0 < 0
    then matchPrefix
    else matchParam m0 str0
  where
    matchPrefix =
      case SeqM.matchStr (_tmch_prefixMatcher m0) str0 of
        ConsumerFinish m _prev rest len ->
          matchParam
            ( m0
                { _tmch_prefixMatcher = m,
                  _tmch_prefixMatchOffset = len
                }
            )
            rest
        ConsumerContinue m ->
          ConsumerContinue (m0 {_tmch_prefixMatcher = m})
    matchParam m1 str =
      case BS.uncons str of
        Nothing -> ConsumerContinue m1
        Just (x, xs) ->
          if x == 104 || x == 108
            then
              ConsumerFinish
                (matcherReset m1)
                (BS.take (BS.length str0 - BS.length xs) str0)
                xs
                (x == 104, _tmch_prefixMatchOffset m1 + 1)
            else ConsumerContinue (matcherReset m1)

matcherReset :: TuiModeMatcher -> TuiModeMatcher
matcherReset TuiModeMatcher {..} =
  TuiModeMatcher
    { _tmch_prefixMatcher = SeqM.matcherReset _tmch_prefixMatcher,
      _tmch_prefixMatchOffset = -1
    }
