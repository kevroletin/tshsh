{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Tshsh.Muxer.Body where

import Control.Lens
import Control.Monad
import Data.BufferSlice (BufferSlice (..), SliceList (..))
import qualified Data.BufferSlice as BufferSlice
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Strict.Tuple
import Data.String.AnsiEscapeCodes.Strip.Text
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Foreign
import GHC.Base (String)
import Lang.Coroutine.CPS
import Lang.Coroutine.CPS.Folds
import Matcher.ByteString
import Matcher.Result
import Protolude hiding (hPutStrLn, log, tryIO)
import System.Console.ANSI
import System.Console.Terminal.Size
import System.IO hiding (hPutStr)
import System.Posix
import System.Posix.Signals.Exts
import System.Process
import Tshsh.Commands
import Tshsh.Muxer.Types
import Tshsh.Program.SyncCwd
import Tshsh.Puppet

import Spec.CPS.Folds

syncTerminalSize :: String -> IO ()
syncTerminalSize pts = do
  Just (Window h w :: Window Int) <- size
  -- TODO: link c code
  _ <- system ("stty -F " <> pts <> " cols " <> show w <> " rows " <> show h)
  pure ()

copyToXClipboard :: Text -> IO ()
copyToXClipboard str = do
  (Just inP, _, _, _) <- createProcess $ (proc "xclip" ["-selection", "clipboard", "-in"]) {std_in = CreatePipe}
  T.hPutStr inP str
  hClose inP

bsDropEnd :: Int -> ByteString -> ByteString
bsDropEnd n xs = BS.take (BS.length xs - n) xs

-- strip 1st and the last lines, strip ansi escape sequences
stripCmdOut :: BufferSlice.SliceList -> Text
stripCmdOut list =
  let bs = BufferSlice.listConcat list
      bs' = BS.drop 1 . Protolude.snd . BS.break (== 10) . bsDropEnd 1 . Protolude.fst . BS.breakEnd (== 10) $ bs
   in T.strip . stripAnsiEscapeCodes $ cs bs'

data ParsePromptSt = ParsePromptSt
  { _pps_promptMatcher :: SomeMatcher,
    _pps_clrScrMatcher :: SomeMatcher,
    _pps_mode :: PuppetMode
  }
  deriving Show

setFst' :: a -> Pair a b -> Pair a b
setFst' a (_ :!: b) = a :!: b
{-# INLINE setFst' #-}

setSnd' :: b -> Pair a b -> Pair a b
setSnd' b (a :!: _) = a :!: b
{-# INLINE setSnd' #-}

-- This config implemented as a class to be able to specialize
class RaceMatchersDataCfg a where
  onData :: BufferSlice -> a
  onFstEv :: Int -> a
  onSndEv :: Int -> a

class RaceMatchersStateCfg st where
  fstMatcher :: Lens' st SomeMatcher
  sndMatcher :: Lens' st SomeMatcher

instance RaceMatchersDataCfg SegmentedOutput where
  onData = Data
  onFstEv = Prompt
  onSndEv = const TuiMode

instance RaceMatchersStateCfg PuppetState where
  fstMatcher = ps_parser
  sndMatcher = ps_clrScrParser

instance RaceMatchersStateCfg (Pair SomeMatcher SomeMatcher) where
  fstMatcher f (a :!: b) = (:!: b) <$> f a
  sndMatcher f (a :!: b) = (a :!:) <$> f b

-- | Split input based on matches from given matchers
--
-- Let's say we have two matchers to detect a prompt $ and a clear screen sequence.
-- raceMatchersP splits input into smaller chunks each time the first or the
-- second matcher fires. In between it inserts messages to indicate that a match
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
-- the difficult part of the implementation is to avoid running the same matcher
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
-- than matching on strings due to memChr optimization). Or it would use
-- matchStr, but would sometimes run matchStr several times on parts of the
-- same input).
raceMatchersP :: forall st out m .
  (RaceMatchersStateCfg st, RaceMatchersDataCfg out) =>
  Program st BufferSlice out m
raceMatchersP =
  let feedM m0 putSt onOut bs@(BufferSlice id buf size) cont =
        let str = BufferSlice.sliceToByteString bs
         in case matchStr m0 str of
              NoMatch newM ->
                ModifyState (putSt .~ newM) $
                if BufferSlice.sliceNull bs
                   then cont
                   else Output (onData bs) cont
              Match newM len prev rest ->
                ModifyState (putSt .~ newM) $
                Output (onData $ BufferSlice.sliceTake (BS.length prev) bs) $
                Output (onOut len) $
                feedM newM putSt onOut (BufferSlice.sliceDrop (BS.length prev) bs) cont
      go bs0@(BufferSlice id buf size) =
        let str = BS.fromForeignPtr buf 0 size
         in GetState $ \st ->
              case matchStr (st ^. fstMatcher) str of
                NoMatch newFstM ->
                  ModifyState (fstMatcher .~ newFstM) $
                  feedM (st ^. sndMatcher)
                        sndMatcher
                        onSndEv
                        bs0 $
                  raceMatchersP
                Match newFstM lenFst prevFst restFst ->
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
                    Match newSndM lenSnd prevSnd restSnd ->
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
{-# SPECIALIZE raceMatchersP :: forall m . Program PuppetState BufferSlice SegmentedOutput m #-}

accumCmdOutP :: Program PuppetState SegmentedOutput SliceList IO
accumCmdOutP =
  WaitInput $ \i ->
  GetState $ \st@PuppetState{..} ->
    case i of
      Data bs ->
        if st ^. ps_mode == PuppetModeRepl
          then
            PutState (st & ps_currCmdOut %~ (`BufferSlice.listAppendEnd` bs))
            accumCmdOutP
          else
            accumCmdOutP
      Prompt len ->
        if st ^. ps_mode == PuppetModeRepl
          then
            let res = BufferSlice.listDropEnd len _ps_currCmdOut in
            PutState (st { _ps_prevCmdOut = res,
                           _ps_currCmdOut = BufferSlice.listEmpty }) $
            Output res
            accumCmdOutP
          else
            PutState (st { _ps_mode = PuppetModeRepl })
            accumCmdOutP
      TuiMode ->
        if st ^. ps_mode == PuppetModeRepl
          then
            PutState (st { _ps_mode = PuppetModeTUI,
                           _ps_currCmdOut = BufferSlice.listEmpty })
            accumCmdOutP
          else
            accumCmdOutP

stripCmdOutP :: Program PuppetState SliceList Text IO
stripCmdOutP =
  WaitInput $ \i ->
    Output (stripCmdOut i) stripCmdOutP

muxBody :: MuxEnv -> MuxState -> MuxCmd -> IO MuxState
muxBody env st (TermInput (BufferSlice _ buff size)) = do
  let h = env ^. menv_currentPuppet st . pup_inputH
  withForeignPtr buff $ \ptr -> do
    hPutBuf h ptr size
    pure st
muxBody env st (PuppetOutput puppetIdx inp@(BufferSlice inpSliceId buf size)) = do
  let str0 = BS.fromForeignPtr buf 0 size

  let runProgram p = do
        let onOut (i, x) = do
              -- TODO: should we push output into muxer queue?
              let h = env ^. menv_puppets . pupIdx i . pup_inputH
              BS.hPut h x
        feedInputM onOut (puppetIdx, BS.copy str0) p >>= \case
          Cont p' -> pure (Just p')
          Res r -> do
            hPutStrLn stderr $ "Program terminated with: " <> show r
            pure Nothing

  when (puppetIdx == st ^. mst_currentPuppetIdx) $
    BS.hPut stdout str0

  let onOut x = do hPutStr stderr ("-> " :: Text)
                   hPrint stderr x
  let currP = st ^. mst_currentPuppet
  Cont (newPup :!: newModeP) <- feedInputM onOut inp (currP :!: (currP ^. ps_modeP))

  nextCp <- join <$> traverse runProgram (st ^. mst_currentProgram)

  pure
    ( st & mst_currentProgram .~ nextCp
         & mst_currentPuppet .~ (newPup & ps_modeP .~ newModeP)
    )
muxBody env st WindowResize = do
  let pup = env ^. menv_currentPuppet st
  syncTerminalSize (pup ^. pup_pts)
  pure st
muxBody env st0 SwitchPuppet = do
  let idx = nextPuppet (st0 ^. mst_currentPuppetIdx)
  let st = st0 & mst_currentPuppetIdx .~ idx
  let currP = env ^. menv_currentPuppet st

  -- TODO: a hack. Finxing paste from X clipboard in ghci
  -- Paste stops working in GHCI if bracket mode is enabled. Zsh enables bracket paste
  -- mode each time it prints a prompt (at least in our setup with zprezto).
  -- see https://cirw.in/blog/bracketed-paste
  BS.hPut stdout ("\x1b[?2004l" :: BS.ByteString)

  let copyPrevCmdC = Lift . copyToXClipboard . stripCmdOut $ st0 ^. mst_currentPuppet . ps_prevCmdOut
      copyPrevCmd = copyPrevCmdC $ \_ -> Finish (Right ())
      program = () :!: syncCwdP env idx copyPrevCmd

  let (from :!: to) = st0 ^. mst_sortedPuppets
  mProgram <- case (_ps_mode from, _ps_mode to) of
    (_, PuppetModeTUI) -> do
      -- returning into tui -> send C-l to with the hope that tui app will redraw itself
      BS.hPut (currP ^. pup_inputH) ("\f" :: BS.ByteString)
      pure Nothing
    (PuppetModeRepl, PuppetModeRepl) -> do
      -- switching between repls -> send C-c with the hope that repl will render a prompt
      signalProcess keyboardSignal (currP ^. pup_pid)
      pure (Just program)
    (PuppetModeTUI, PuppetModeRepl) -> do
      -- clear tui interface, try to redraw repl prompt by sending C-c
      BS.hPut stdout "\ESC[H\ESC[2J" -- move cursor to (0,0) clearScreen
      showCursor
      signalProcess keyboardSignal (currP ^. pup_pid)
      pure (Just program)

  pure (st & mst_currentProgram .~ mProgram)

