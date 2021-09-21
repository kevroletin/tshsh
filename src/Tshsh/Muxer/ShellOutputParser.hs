{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Tshsh.Muxer.ShellOutputParser where

import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Coerce
import Data.String.AnsiEscapeCodes.Strip.ByteString
import Protolude
import Tshsh.Data.BufferSlice (BufferSlice (..), SliceList (..))
import qualified Tshsh.Data.BufferSlice as BufferSlice
import Tshsh.Lang.Coroutine.CPS
import Tshsh.Puppet
import Tshsh.Stream

-- This config implemented as a class toSt be able toSt specialize
class RaceMatchersDataCfg a where
  type FstParam a
  type SndParam a
  onData :: BufferSlice -> a
  onFstEv :: FstParam a -> a
  onSndEv :: SndParam a -> a

class RaceMatchersDataCfg a => RaceMatchersStateCfg st a | st -> a where
  fstMatcher :: Lens' st (StreamConsumer ByteString (FstParam a))
  sndMatcher :: Lens' st (StreamConsumer ByteString (SndParam a))

instance RaceMatchersDataCfg ShellModeAndOutput where
  type FstParam ShellModeAndOutput = Int
  type SndParam ShellModeAndOutput = (Bool, Int)
  onData = Data
  onFstEv len = Prompt len
  onSndEv (b, len) = TuiMode b len

instance RaceMatchersStateCfg OutputParserSt ShellModeAndOutput where
  fstMatcher = op_promptMatcher
  sndMatcher = op_tuiModeMatcher

-- | Split input based on matches fromSt given matchers
--
-- Let's say we have two matchers to detect a prompt $ and a clear screen sequence.
-- raceMatchersP splits input into smaller chunks each time the first or the
-- second matcher fires. In between it inserts messages to indicate that a match
-- happened. For example given input (1) it produces a sequence (2)
-- (1) [ |   $     $   \ESC[?1049h | ]
-- (2) [ |   $|
--     ,     PromptDetected
--     ,     |     $|
--     ,           PromptDetected
--     ,           |   \ESC[?1049h|
--     ,                         TuiModeDetected
--     ,                         | |
--     ]
--
-- the difficult part of the implementation is to avoid running the same matcher
-- more than once on the same input
--
-- in the case of (1) we run prompt matcher and tuiMode matcher one after the
-- other and discover that both matched, but a prompt appeared earlier in
-- the input. That means that in the portion of the input until tuiMode match
-- there might be multiple occurrences of a prompt but no occurrences of tuiMode.
--
-- The implementation would be simple if we would combine two matchers into
-- a single one. But it likely would be less efficient because it would either
-- use matcherStep and consume the input element by element (which is slower
-- than matching on strings due to memChr optimization). Or it would use
-- consume, but would sometimes run consume several times on parts of the
-- same input).

{- ORMOLU_DISABLE -}
raceMatchersP :: forall st out m .
  (RaceMatchersStateCfg st out, RaceMatchersDataCfg out) =>
  Program st BufferSlice out m ()
raceMatchersP =
  let feedM m0 putSt onOut bs cont =
        let str = BufferSlice.sliceToByteString bs
         in case consume m0 str of
              ConsumerContinue newM ->
                ModifyState (putSt .~ newM) $
                if BufferSlice.sliceNull bs
                   then cont
                   else Output (onData bs) cont
              ConsumerFinish newM prev _rest res ->
                ModifyState (putSt .~ newM) $
                Output (onData $ BufferSlice.sliceTake (BS.length prev) bs) $
                Output (onOut res) $
                feedM newM putSt onOut (BufferSlice.sliceDrop (BS.length prev) bs) cont
      go bs0 =
        let str = BufferSlice.sliceToByteString bs0
         in GetState $ \st ->
              case consume (st ^. fstMatcher) str of
                ConsumerContinue newFstM ->
                  ModifyState (fstMatcher .~ newFstM) $
                  feedM (st ^. sndMatcher)
                        sndMatcher
                        onSndEv
                        bs0 $
                  raceMatchersP
                ConsumerFinish newFstM prevFst _restFst resFst ->
                  ModifyState (fstMatcher .~ newFstM) $
                  case consume (st ^. sndMatcher) str of
                    ConsumerContinue newSndM ->
                      ModifyState (sndMatcher .~ newSndM) $
                      Output (onData (BufferSlice.sliceTake (BS.length prevFst) bs0)) $
                      Output (onFstEv resFst) $
                      feedM newFstM
                            fstMatcher
                            onFstEv
                            (BufferSlice.sliceDrop (BS.length prevFst) bs0) $
                      raceMatchersP
                    ConsumerFinish newSndM prevSnd _restSnd resSnd ->
                      if BS.length prevSnd < BS.length prevFst
                        then
                          ModifyState (sndMatcher .~ newSndM) $
                          Output (onData (BufferSlice.sliceTake (BS.length prevSnd) bs0)) $
                          Output (onSndEv resSnd) $
                          feedM newSndM
                                sndMatcher
                                onSndEv
                                ( bs0 & BufferSlice.sliceTake (BS.length prevFst)
                                      & BufferSlice.sliceDrop (BS.length prevSnd)) $
                          Output (onFstEv resFst) $
                          go (BufferSlice.sliceDrop (BS.length prevFst) bs0)
                        else
                          ModifyState (fstMatcher .~ newFstM) $
                          Output (onData (BufferSlice.sliceTake (BS.length prevFst) bs0)) $
                          Output (onFstEv resFst) $
                          feedM newFstM
                                fstMatcher
                                onFstEv
                                (bs0 & BufferSlice.sliceTake (BS.length prevSnd)
                                     & BufferSlice.sliceDrop (BS.length prevFst)) $
                          Output (onSndEv resSnd) $
                          go (BufferSlice.sliceDrop (BS.length prevSnd) bs0)
   in WaitInput go
{-# SPECIALIZE raceMatchersP :: Program OutputParserSt BufferSlice ShellModeAndOutput IO () #-}
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
accumCmdOutP :: Program OutputParserSt ShellModeAndOutput RawCmdResult IO ()
accumCmdOutP =
  WaitInput $ \i ->
  GetState $ \st@OutputParserSt{..} ->
    case i of
      Data bs ->
        if st ^. op_mode == PuppetModeRepl
          then
            PutState (st & op_currCmdOut %~ (\x -> coerce (BufferSlice.listAppendEnd (coerce x) bs)))
            accumCmdOutP
          else
            accumCmdOutP
      Prompt len ->
        if st ^. op_mode == PuppetModeRepl
          then
            let res = BufferSlice.listDropEnd len (coerce _op_currCmdOut) in
            PutState (st {_op_currCmdOut = coerce BufferSlice.listEmpty }) $
            Output (RawCmdResult res)
            accumCmdOutP
          else
            PutState (st { _op_mode = PuppetModeRepl })
            accumCmdOutP
      TuiMode enable _ ->
        if enable && st ^. op_mode == PuppetModeRepl
          then
            PutState (st { _op_mode = PuppetModeTUI,
                           _op_currCmdOut = coerce BufferSlice.listEmpty })
            accumCmdOutP
          else
            PutState (st { _op_mode = if enable then PuppetModeTUI else PuppetModeRepl })
            accumCmdOutP
{- ORMOLU_ENABLE -}

bsDropEnd :: Int -> ByteString -> ByteString
bsDropEnd n xs = BS.take (BS.length xs - n) xs

-- TODO: make configurable per puppet
-- strip 1st and the last lines, strip ansi escape sequences
stripCmdOut :: RawCmdResult -> StrippedCmdResult
stripCmdOut (RawCmdResult sl) =
  let bs = BufferSlice.listConcat sl
      bs' = BS.drop 1 . Protolude.snd . BS.break (== 10) . bsDropEnd 1 . Protolude.fst . BS.breakEnd (== 10) $ bs
   in StrippedCmdResult . C8.strip . stripAnsiEscapeCodes $ bs'

stripCmdOutP :: Program OutputParserSt RawCmdResult StrippedCmdResult IO ()
stripCmdOutP =
  WaitInput $ \i ->
    Output (stripCmdOut i) stripCmdOutP
