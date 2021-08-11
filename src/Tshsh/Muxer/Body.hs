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
import qualified System.Console.Terminal.Size as TerminalSize
import System.IO hiding (hPutStr)
import System.Posix
import System.Process
import Tshsh.Commands
import Tshsh.Muxer.Types
import Tshsh.Program.SyncCwd
import Tshsh.Puppet

syncTtySize :: String -> IO ()
syncTtySize pts = do
  Just (TerminalSize.Window h w :: TerminalSize.Window Int) <- TerminalSize.size
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

-- TODO: what about multiline commands?
-- strip 1st and the last lines, strip ansi escape sequences
stripCmdOut :: BufferSlice.SliceList -> Text
stripCmdOut sl =
  let bs = BufferSlice.listConcat sl
      bs' = BS.drop 1 . Protolude.snd . BS.break (== 10) . bsDropEnd 1 . Protolude.fst . BS.breakEnd (== 10) $ bs
   in T.strip . stripAnsiEscapeCodes $ cs bs'

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
      go bs0@(BufferSlice _ buf size) =
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
{-# SPECIALIZE raceMatchersP :: forall m . Program PuppetState BufferSlice SegmentedOutput m #-}
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
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
{- ORMOLU_ENABLE -}

stripCmdOutP :: Program PuppetState SliceList CmdResultOutput IO
stripCmdOutP =
  WaitInput $ \i ->
    Output (CmdResultOutput $ stripCmdOut i) stripCmdOutP

logSliceList :: Text -> Int -> SliceList -> IO ()
logSliceList msg n bl = do
  hPutStr stderr msg
  hPutStr stderr (show $ BufferSlice.listConcat (BufferSlice.listTake n bl) :: Text)
  when (BufferSlice.listLength bl > n) $
    hPutStr stderr ("..." :: Text)
  hPutStr stderr ("\n" :: Text)

-- manually loop over output of commands output parser and feed into sync cwd
pipeInput ::
  MuxState ->
  PuppetIdx ->
  Maybe inp ->
  Pair
    (ProgramSt st1 inp SliceList IO)
    (Maybe (ProgramSt () (PuppetIdx, CmdResultOutput) (PuppetIdx, ByteString) IO)) ->
  IO
    ( Pair
        (ProgramSt st1 inp SliceList IO)
        (Maybe (ProgramSt () (PuppetIdx, CmdResultOutput) (PuppetIdx, ByteString) IO))
    )
pipeInput st puppetIdx = loop
  where
    onOut (i, x) = do
      hPutStr stderr $ "~> " <> (show (i, x) :: Text) <> "\n"
      case st ^. mst_puppetSt . pupIdx i . ps_process of
        Just p -> BS.hPut (_pp_inputH p) x
        Nothing -> pure ()
    loop i (producer :!: mConsumer) =
      step i producer >>= \case
        ContOut (Just o) prodCont -> do
          logSliceList (show puppetIdx <> "> ") 40 o
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

runMuxPrograms :: MuxState -> PuppetIdx -> Maybe BufferSlice -> IO MuxState
runMuxPrograms st puppetIdx mInp = do
  let thisPuppet = st ^. mst_puppetSt . pupIdx puppetIdx
  let cmdOutPSt = thisPuppet :!: thisPuppet ^. ps_cmdOutP
      mSyncCwdPSt = (() :!:) <$> _mst_syncCwdP st
  ((newThisPup :!: newCmdOutP) :!: newMuxProg) <-
    case mInp of
      Nothing ->
        pipeInput st puppetIdx Nothing (cmdOutPSt :!: mSyncCwdPSt)
      (Just inp) ->
        pipeInput st puppetIdx (Just inp)
          =<< pipeInput st puppetIdx Nothing (cmdOutPSt :!: mSyncCwdPSt)
  pure
    ( st & mst_puppetSt . pupIdx puppetIdx .~ (newThisPup & ps_cmdOutP .~ newCmdOutP)
        & mst_syncCwdP .~ ((^. _2) <$> newMuxProg)
    )

whenJustP :: Maybe (a -> a) -> a -> a
whenJustP Nothing cont = cont
whenJustP (Just act) cont = act cont

switchPuppets :: MuxEnv -> MuxState -> IO (Maybe MuxState)
switchPuppets env st0 = do
  let newIdx = nextPuppet (st0 ^. mst_currentPuppetIdx)
  let st = st0 {_mst_currentPuppetIdx = newIdx}
  let (toSt :!: fromSt) = st ^. mst_sortedPuppets
  let (toPup :!: _) = env ^. menv_sortedPuppets st

  (toProc, startedNewProc, newSt) <-
    case _ps_process toSt of
      Just pid -> pure (pid, False, st)
      Nothing -> do
        Protolude.putStrLn ("\n\rStarting a new process..\r" :: Text)
        pid <- _pup_startProcess toPup
        pure (pid, True, st & mst_currentPuppet . ps_process .~ Just pid)

  let waitPrompt cont = WaitInput $ \_ -> cont
      copyPrevCmdC = liftP_ (copyToXClipboard . stripCmdOut $ _ps_prevCmdOut fromSt)
      mSyncCwdC =
        case _ps_process fromSt of
          Nothing -> Nothing
          Just fromPid -> Just (syncCwdC (_pp_pid toProc :!: _pp_pid fromPid) env newIdx)
      selectInp (idx, x) = if idx == newIdx then Just x else Nothing
      clearPromptC = _pup_cleanPromptC toPup toProc selectInp (newIdx,)
      restoreTuiC = _pup_restoreTuiC toPup toProc selectInp (newIdx,)
      {- ORMOLU_DISABLE -}
      program =
        case (_ps_mode fromSt, _ps_mode toSt) of
          (_, PuppetModeTUI) ->
            unlessP startedNewProc restoreTuiC $
            copyPrevCmdC
            finishP
          (fromMode, PuppetModeRepl) ->
            whenP (fromMode == PuppetModeTUI)
              ( liftP_                              -- clear tui interface
                ( do BS.hPut stdout "\ESC[H\ESC[2J" -- move cursor toSt (0,0) clearScreen
                     showCursor
                 )
              ) $
            unlessP startedNewProc clearPromptC $
            waitPrompt $
            whenJustP mSyncCwdC $
            copyPrevCmdC
            finishP
      {- ORMOLU_ENABLE -}
  Just <$> runMuxPrograms (newSt & mst_syncCwdP ?~ program) newIdx Nothing

muxBody :: MuxEnv -> MuxState -> MuxCmd -> IO (Maybe MuxState)
muxBody _env st (TermInput (BufferSlice _ buf size)) = do
  case st ^. mst_currentPuppet . ps_process of
    Nothing -> pure ()
    Just p ->
      withForeignPtr buf $ \ptr -> hPutBuf (_pp_inputH p) ptr size
  pure (Just st)
muxBody _env st (PuppetOutput puppetIdx inp@(BufferSlice _ buf size)) = do
  when (puppetIdx == st ^. mst_currentPuppetIdx) $
    withForeignPtr buf $ \ptr -> do
      hPutBuf stdout ptr size
  Just <$> runMuxPrograms st puppetIdx (Just inp)
muxBody _env st WindowResize = do
  traverse_ syncTtySize (st ^? mst_currentPuppet . ps_process . _Just . pp_pts)
  traverse_ syncTtySize (st ^? mst_otherPuppet . ps_process . _Just . pp_pts)
  pure (Just st)
muxBody env st0 SwitchPuppet = switchPuppets env st0
muxBody env st0 (ChildExited exitedPid) = do
  let (currSt :!: otherSt) = st0 ^. mst_sortedPuppets
  let (currPup :!: otherPup) = env ^. menv_sortedPuppets st0
  case _ps_process currSt of
    Nothing -> pure Nothing
    Just currPid ->
      if _pp_pid currPid == exitedPid
        then
          if _mst_keepAlive st0 || isJust (_ps_process otherSt)
            then
              switchPuppets
                env
                ( st0 & mst_currentPuppet .~ _pup_initState currPup
                    & mst_syncCwdP .~ Nothing
                )
            else pure Nothing
        else case _ps_process otherSt of
          Nothing ->
            pure (Just st0)
          Just otherPid ->
            if _pp_pid otherPid == exitedPid
              then
                pure . Just $
                  st0 & mst_otherPuppet .~ _pup_initState otherPup
                    & mst_syncCwdP .~ Nothing
              else pure (Just st0)
