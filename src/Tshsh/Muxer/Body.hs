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
import Data.Strict.Tuple.Extended
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

-- This config implemented as a class toSt be able toSt specialize
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
raceMatchersP :: forall st msg out m .
  (RaceMatchersStateCfg st, RaceMatchersDataCfg msg) =>
  (msg -> ProgramCont' st BufferSlice out m) ->
  Program st BufferSlice out m
raceMatchersP consumer =
  let feedM m0 putSt onOut bs@(BufferSlice id buf size) cont =
        let str = BufferSlice.sliceToByteString bs
         in case matchStr m0 str of
              NoMatch newM ->
                ModifyState (putSt .~ newM) $
                if BufferSlice.sliceNull bs
                   then cont
                   else consumer (onData bs) cont
              Match newM len prev rest ->
                ModifyState (putSt .~ newM) $
                consumer (onData $ BufferSlice.sliceTake (BS.length prev) bs) $
                consumer (onOut len) $
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
                  raceMatchersP consumer
                Match newFstM lenFst prevFst restFst ->
                  ModifyState (fstMatcher .~ newFstM) $
                  case matchStr (st ^. sndMatcher) str of
                    NoMatch newSndM ->
                      ModifyState (sndMatcher .~ newSndM) $
                      consumer (onData (BufferSlice.sliceTake (BS.length prevFst) bs0)) $
                      consumer (onFstEv lenFst) $
                      feedM newFstM
                            fstMatcher
                            onFstEv
                            (BufferSlice.sliceDrop (BS.length prevFst) bs0) $
                      raceMatchersP consumer
                    Match newSndM lenSnd prevSnd restSnd ->
                      if BS.length prevSnd < BS.length prevFst
                        then
                          ModifyState (sndMatcher .~ newSndM) $
                          consumer (onData (BufferSlice.sliceTake (BS.length prevSnd) bs0)) $
                          consumer (onSndEv lenSnd) $
                          feedM newSndM
                                sndMatcher
                                onSndEv
                                ( bs0 & BufferSlice.sliceTake (BS.length prevFst)
                                      & BufferSlice.sliceDrop (BS.length prevSnd)) $
                          consumer (onFstEv lenFst) $
                          go (BufferSlice.sliceDrop (BS.length prevFst) bs0)
                        else
                          ModifyState (fstMatcher .~ newFstM) $
                          consumer (onData (BufferSlice.sliceTake (BS.length prevFst) bs0)) $
                          consumer (onFstEv lenSnd) $
                          feedM newFstM
                                fstMatcher
                                onFstEv
                                (bs0 & BufferSlice.sliceTake (BS.length prevSnd)
                                     & BufferSlice.sliceDrop (BS.length prevFst)) $
                          consumer (onSndEv lenSnd) $
                          go (BufferSlice.sliceDrop (BS.length prevSnd) bs0)
   in WaitInput go
-- {-# SPECIALIZE raceMatchersP :: forall m . Program PuppetState BufferSlice SegmentedOutput m #-}

maybeStartProcess :: Either (IO (Pair ProcessHandle ProcessID)) (Pair ProcessHandle ProcessID)
                  -> IO (Pair ProcessHandle ProcessID)
maybeStartProcess x =
    case x of
      Left startProc -> startProc
      Right pids -> pure pids

startProcesses :: MuxState -> IO (MuxState, Pair (Pair ProcessHandle ProcessID) (Pair ProcessHandle ProcessID))
startProcesses mux = do
  let (st1 :!: st2) = _mst_puppetSt mux
  case (_ps_process st1, _ps_process st2) of
    (Right a, Right b) -> pure (mux, sortPup_ (_mst_currentPuppetIdx mux) (a :!: b))
    (ma, mb) -> do
      a <- maybeStartProcess ma
      b <- maybeStartProcess mb
      pure ( mux { _mst_puppetSt = ( st1 { _ps_process = Right a } :!:
                                     st2 { _ps_process = Right b } ) },
             sortPup_ (_mst_currentPuppetIdx mux) (a :!: b) )

accumCmdOutP :: SegmentedOutput -> ProgramCont' PuppetState inp SliceList IO
accumCmdOutP i cont =
  GetState $ \st@PuppetState{..} ->
    case i of
      Data bs ->
        if st ^. ps_mode == PuppetModeRepl
          then
            PutState (st & ps_currCmdOut %~ (`BufferSlice.listAppendEnd` bs))
            cont
          else
            cont
      Prompt len ->
        if st ^. ps_mode == PuppetModeRepl
          then
            let res = BufferSlice.listDropEnd len _ps_currCmdOut in
            PutState (st { _ps_prevCmdOut = res,
                           _ps_currCmdOut = BufferSlice.listEmpty }) $
            Output res
            cont
          else
            PutState (st { _ps_mode = PuppetModeRepl })
            cont
      TuiMode ->
        if st ^. ps_mode == PuppetModeRepl
          then
            PutState (st { _ps_mode = PuppetModeTUI,
                           _ps_currCmdOut = BufferSlice.listEmpty })
            cont
          else
            cont

-- manually loop over output of commands output parser and feed into sync cwd
pipeInput ::
  MuxEnv
  -> PuppetIdx
  -> Maybe inp
  -> Pair (ProgramSt st1 inp SliceList IO)
          (Maybe (ProgramSt () (PuppetIdx, CmdResultOutput) (PuppetIdx, ByteString) IO))
  -> IO (Pair
          (ProgramSt st1 inp SliceList IO)
          (Maybe (ProgramSt () (PuppetIdx, CmdResultOutput) (PuppetIdx, ByteString) IO)))
pipeInput env puppetIdx = loop
  where
    onOut (i, x) = do
      hPutStr stderr $ "-> Send " <> (show (i, x) :: Text) <> "\n"
      let h = env ^. menv_puppets . pupIdx i . pup_inputH
      BS.hPut h x
    loop i (producer :!: mConsumer) =
      step i producer >>= \case
        ContOut (Just o) prodCont -> do
          hPutStr stderr ("-> " :: Text)
          hPrint stderr o
          case mConsumer of
            Nothing -> loop Nothing (prodCont :!: Nothing)
            Just consumer ->
              feedInputM onOut (puppetIdx, CmdResultOutput . stripCmdOut $ o) consumer >>= \case
                  Cont consCont -> loop Nothing (prodCont :!: Just consCont)
                  Res r -> do
                    hPutStrLn stderr $ "Sync cwd terminated with: " <> show r
                    loop Nothing (prodCont :!: Nothing)
        ContOut Nothing prodCont ->
          case mConsumer of
            Nothing -> pure (prodCont :!: mConsumer)
            Just consumer ->
              eatOutputsM onOut consumer >>= \case
                  Cont consCont -> pure (prodCont :!: Just consCont)
                  Res r -> do
                    hPutStrLn stderr $ "Sync cwd terminated with: " <> show r
                    pure (prodCont :!: Nothing)
        ResOut (_ :!: r) -> panic ("oops, input parser exited with " <> show r)

runMuxPrograms :: MuxEnv -> MuxState -> PuppetIdx -> Maybe BufferSlice -> IO MuxState
runMuxPrograms env st puppetIdx mInp = do
  let thisPuppet = st ^. mst_puppetSt . pupIdx puppetIdx
  let cmdOutPSt = thisPuppet :!: thisPuppet ^. ps_cmdOutP
      mSyncCwdPSt = (():!:) <$> _mst_syncCwdP st
  ((newThisPup :!: newCmdOutP) :!: newMuxProg) <-
    case mInp of
      Nothing ->
        pipeInput env puppetIdx Nothing (cmdOutPSt :!: mSyncCwdPSt)
      (Just inp) ->
        pipeInput env puppetIdx (Just inp) =<<
          pipeInput env puppetIdx Nothing (cmdOutPSt :!: mSyncCwdPSt)
  pure
    ( st & mst_puppetSt . pupIdx puppetIdx .~ (newThisPup & ps_cmdOutP .~ newCmdOutP)
         & mst_syncCwdP .~ ((^. _2) <$> newMuxProg)
    )

muxBody :: MuxEnv -> MuxState -> MuxCmd -> IO MuxState
muxBody env st (TermInput (BufferSlice _ buf size)) = do
  let h = env ^. menv_currentPuppet st . pup_inputH
  withForeignPtr buf $ \ptr -> do
    hPutBuf h ptr size
    pure st
muxBody env st (PuppetOutput puppetIdx inp@(BufferSlice inpSliceId buf size)) = do
  when (puppetIdx == st ^. mst_currentPuppetIdx) $
    withForeignPtr buf $ \ptr -> do
      hPutBuf stdout ptr size
  runMuxPrograms env st puppetIdx (Just inp)
muxBody env st WindowResize = do
  let pup = env ^. menv_currentPuppet st
  syncTerminalSize (pup ^. pup_pts)
  pure st
muxBody env st0 SwitchPuppet = do
  let newIdx = nextPuppet (st0 ^. mst_currentPuppetIdx)
  (st, currPid :!: prevPid)  <- startProcesses (st0 { _mst_currentPuppetIdx = newIdx })
  let (toSt :!: fromSt) = st ^. mst_sortedPuppets
  let to = env ^. menv_currentPuppet st

  -- TODO: a hack. Finxing paste fromSt X clipboard in ghci
  -- Paste stops working in GHCI if bracket mode is enabled. Zsh enables bracket paste
  -- mode each time it prints a prompt (at least in our setup with zprezto).
  -- see https://cirw.in/blog/bracketed-paste
  BS.hPut stdout ("\x1b[?2004l" :: BS.ByteString)

  let copyPrevCmdC = Lift . copyToXClipboard . stripCmdOut $ st0 ^. mst_currentPuppet . ps_prevCmdOut
      copyPrevCmd = copyPrevCmdC $ \_ -> Finish (Right ())
      program = syncCwdP (currPid ^. _2 :!: prevPid ^. _2) env newIdx copyPrevCmd

  mProgram <- case (_ps_mode fromSt, _ps_mode toSt) of
    (_, PuppetModeTUI) -> do
      -- returning into tui
      -- send ESC in case it's vim and it's in input mode
      -- -> send C-l toSt with the hope that tui app will redraw itself
      BS.hPut (to ^. pup_inputH) ("\ESC" :: BS.ByteString)
      BS.hPut (to ^. pup_inputH) ("\f" :: BS.ByteString)
      BS.hPut (to ^. pup_inputH) ("\f" :: BS.ByteString)
      pure Nothing
    (PuppetModeRepl, PuppetModeRepl) -> do
      -- switching between repls -> send C-c with the hope that repl will render a prompt
      signalProcess keyboardSignal (currPid ^. _2)
      pure (Just program)
    (PuppetModeTUI, PuppetModeRepl) -> do
      -- clear tui interface, try toSt redraw repl prompt by sending C-c
      BS.hPut stdout "\ESC[H\ESC[2J" -- move cursor toSt (0,0) clearScreen
      showCursor
      signalProcess keyboardSignal (currPid ^. _2)
      pure (Just program)

  runMuxPrograms env (st & mst_syncCwdP .~ mProgram) newIdx Nothing
