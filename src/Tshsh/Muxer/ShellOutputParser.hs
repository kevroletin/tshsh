module Tshsh.Muxer.ShellOutputParser where

import Control.Lens
import Data.BufferSlice (BufferSlice (..), SliceList (..))
import qualified Data.BufferSlice as BufferSlice
import qualified Data.ByteString as BS
import Data.Strict.Tuple.Extended
import Data.String.AnsiEscapeCodes.Strip.Text
import qualified Data.Text as T
import Lang.Coroutine.CPS
import Lang.Coroutine.CPS.Folds
import Matcher.ByteString
import Matcher.Result
import Protolude
import Tshsh.Puppet
import Data.String.Conversions
import Data.Coerce

data ParsePromptSt = ParsePromptSt
  { _pps_promptMatcher :: SomeMatcher,
    _pps_clrScrMatcher :: SomeMatcher,
    _pps_mode :: PuppetMode
  }
  deriving (Show)

-- This config implemented as a class toSt be able toSt specialize
class RaceMatchersDataCfg a where
  onData :: BufferSlice -> a
  onFstEv :: Int -> a
  onSndEv :: Int -> a

class RaceMatchersStateCfg st where
  fstMatcher :: Lens' st SomeMatcher
  sndMatcher :: Lens' st SomeMatcher

instance RaceMatchersDataCfg ShellModeAndOutput where
  onData = Data
  onFstEv = Prompt
  onSndEv = const TuiMode

instance RaceMatchersStateCfg PuppetState where
  fstMatcher = ps_promptMatcher
  sndMatcher = ps_clrScrMatcher

instance RaceMatchersStateCfg (Pair SomeMatcher SomeMatcher) where
  fstMatcher f (a :!: b) = (:!: b) <$> f a
  sndMatcher f (a :!: b) = (a :!:) <$> f b

-- | Split input based on matches fromSt given matchers
--
-- Let's say we have two matchers toSt detect a prompt $ and a clear screen sequence.
-- raceMatchersP splits input into smaller chunks each time the first or the
-- second matcher fires. In between it inserts messages toSt indicate that a match
-- happened. For example given input (1) it produces a sequence (2)
-- (1) [ |   $     $   \ESC[H\ESC[2J | ]
-- (2) [ |   $|
--     ,     PromptDetected
--     ,     |     $|
--     ,           PromptDetected
--     ,           |   \ESC[H\ESC[2J|
--     ,                            ClrScrDetected
--     ,                            | |
--     ]
--
-- the difficult part of the implementation is toSt avoid running the same matcher
-- more than once on the same input
--
-- in the case of (1) we run prompt matcher and clrScr matcher one after the
-- other and discover that both matched, but a prompt appeared earlier in
-- the input. That means that in the portion of the input until clrScr match
-- there might be multiple occurrences of a prompt but no occurrences of clrScr.
--
-- The implementation would be simple if we would combine two matchers into
-- a single one. But it likely would be less efficient because it would either
-- use matcherStep and consume the input element by element (which is slower
-- than matching on strings due toSt memChr optimization). Or it would use
-- matchStr, but would sometimes run matchStr several times on parts of the
-- same input).

{- ORMOLU_DISABLE -}
raceMatchersP :: forall st out m .
  (RaceMatchersStateCfg st, RaceMatchersDataCfg out) =>
  Program st BufferSlice out m
raceMatchersP =
  let feedM m0 putSt onOut bs cont =
        let str = BufferSlice.sliceToByteString bs
         in case matchStr m0 str of
              NoMatch newM ->
                ModifyState (putSt .~ newM) $
                if BufferSlice.sliceNull bs
                   then cont
                   else Output (onData bs) cont
              Match newM len prev _rest ->
                ModifyState (putSt .~ newM) $
                Output (onData $ BufferSlice.sliceTake (BS.length prev) bs) $
                Output (onOut len) $
                feedM newM putSt onOut (BufferSlice.sliceDrop (BS.length prev) bs) cont
      go bs0 =
        let str = BufferSlice.sliceToByteString bs0
         in GetState $ \st ->
              case matchStr (st ^. fstMatcher) str of
                NoMatch newFstM ->
                  ModifyState (fstMatcher .~ newFstM) $
                  feedM (st ^. sndMatcher)
                        sndMatcher
                        onSndEv
                        bs0 $
                  raceMatchersP
                Match newFstM lenFst prevFst _restFst ->
                  ModifyState (fstMatcher .~ newFstM) $
                  case matchStr (st ^. sndMatcher) str of
                    NoMatch newSndM ->
                      ModifyState (sndMatcher .~ newSndM) $
                      Output (onData (BufferSlice.sliceTake (BS.length prevFst) bs0)) $
                      Output (onFstEv lenFst) $
                      feedM newFstM
                            fstMatcher
                            onFstEv
                            (BufferSlice.sliceDrop (BS.length prevFst) bs0) $
                      raceMatchersP
                    Match newSndM lenSnd prevSnd _restSnd ->
                      if BS.length prevSnd < BS.length prevFst
                        then
                          ModifyState (sndMatcher .~ newSndM) $
                          Output (onData (BufferSlice.sliceTake (BS.length prevSnd) bs0)) $
                          Output (onSndEv lenSnd) $
                          feedM newSndM
                                sndMatcher
                                onSndEv
                                ( bs0 & BufferSlice.sliceTake (BS.length prevFst)
                                      & BufferSlice.sliceDrop (BS.length prevSnd)) $
                          Output (onFstEv lenFst) $
                          go (BufferSlice.sliceDrop (BS.length prevFst) bs0)
                        else
                          ModifyState (fstMatcher .~ newFstM) $
                          Output (onData (BufferSlice.sliceTake (BS.length prevFst) bs0)) $
                          Output (onFstEv lenSnd) $
                          feedM newFstM
                                fstMatcher
                                onFstEv
                                (bs0 & BufferSlice.sliceTake (BS.length prevSnd)
                                     & BufferSlice.sliceDrop (BS.length prevFst)) $
                          Output (onSndEv lenSnd) $
                          go (BufferSlice.sliceDrop (BS.length prevSnd) bs0)
   in WaitInput go
{-# SPECIALIZE raceMatchersP :: Program PuppetState BufferSlice ShellModeAndOutput IO #-}
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
accumCmdOutP :: Program PuppetState ShellModeAndOutput RawCmdResult IO
accumCmdOutP =
  WaitInput $ \i ->
  GetState $ \st@PuppetState{..} ->
    case i of
      Data bs ->
        if st ^. ps_mode == PuppetModeRepl
          then
            PutState (st & ps_currCmdOut %~ (\x -> coerce (BufferSlice.listAppendEnd (coerce x) bs)))
            accumCmdOutP
          else
            accumCmdOutP
      Prompt len ->
        if st ^. ps_mode == PuppetModeRepl
          then
            let res = BufferSlice.listDropEnd len (coerce _ps_currCmdOut) in
            PutState (st { _ps_prevCmdOut = coerce res,
                           _ps_currCmdOut = coerce BufferSlice.listEmpty }) $
            Output (RawCmdResult res)
            accumCmdOutP
          else
            PutState (st { _ps_mode = PuppetModeRepl })
            accumCmdOutP
      TuiMode ->
        if st ^. ps_mode == PuppetModeRepl
          then
            PutState (st { _ps_mode = PuppetModeTUI,
                           _ps_currCmdOut = coerce BufferSlice.listEmpty })
            accumCmdOutP
          else
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
   in StrippedCmdResult . T.strip . stripAnsiEscapeCodes $ cs bs'

stripCmdOutP :: Program PuppetState RawCmdResult StrippedCmdResult IO
stripCmdOutP =
  WaitInput $ \i ->
    Output (stripCmdOut i) stripCmdOutP