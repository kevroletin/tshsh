{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Tshsh.Muxer.Body where

import Control.Lens
import Control.Monad
import Tshsh.Data.BufferSlice (BufferSlice (..), SliceList (..))
import qualified Tshsh.Data.BufferSlice as BufferSlice
import qualified Data.ByteString as BS
import Data.Strict.Tuple.Extended
import Data.String.Conversions
import qualified Data.Text.IO as T
import Foreign
import GHC.Base (String)
import Tshsh.Lang.Coroutine.CPS
import Tshsh.Lang.Coroutine.CPS.Folds
import Protolude hiding (hPutStrLn, log, tryIO)
import System.Console.ANSI
import qualified System.Console.Terminal.Size as TerminalSize
import System.IO hiding (hPutStr)
import System.Process
import Tshsh.Commands
import Tshsh.Muxer.Types
import Tshsh.Muxer.SyncCwd
import Tshsh.Muxer.ShellOutputParser
import Tshsh.Puppet

syncTtySize :: String -> IO ()
syncTtySize pts = do
  Just (TerminalSize.Window h w :: TerminalSize.Window Int) <- TerminalSize.size
  _ <- callCommand ("stty -F " <> pts <> " cols " <> show w <> " rows " <> show h)
  pure ()

jiggleTtySize :: String -> IO ()
jiggleTtySize pts = do
  Just (TerminalSize.Window h w :: TerminalSize.Window Int) <- TerminalSize.size
  _ <- callCommand ("stty -F " <> pts <> " cols " <> show (w + 1) <> " rows " <> show h)
  _ <- callCommand ("stty -F " <> pts <> " cols " <> show w <> " rows " <> show h)
  pure ()

copyToXClipboard :: Text -> IO ()
copyToXClipboard str = do
  (Just inP, _, _, _) <- createProcess $ (proc "xclip" ["-selection", "clipboard", "-in"]) {std_in = CreatePipe}
  T.hPutStr inP str
  hClose inP

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
    (ProgramSt st1 inp RawCmdResult IO)
    (Maybe (ProgramSt () (PuppetIdx, StrippedCmdResult) (PuppetIdx, ByteString) IO)) ->
  IO
    ( Pair
        (ProgramSt st1 inp RawCmdResult IO)
        (Maybe (ProgramSt () (PuppetIdx, StrippedCmdResult) (PuppetIdx, ByteString) IO))
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
          logSliceList (show puppetIdx <> "> ") 40 (unRawCmdResult o)
          case mConsumer of
            Nothing -> loop Nothing (prodCont :!: Nothing)
            Just consumer ->
              feedInputM onOut (puppetIdx, stripCmdOut o) consumer >>= \case
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
  let cmdOutPSt = thisPuppet :!: thisPuppet ^. ps_outputParser
      mSyncCwdPSt = (() :!:) <$> _mst_syncCwdP st
  ((newThisPup :!: newCmdOutP) :!: newMuxProg) <-
    case mInp of
      Nothing ->
        pipeInput st puppetIdx Nothing (cmdOutPSt :!: mSyncCwdPSt)
      (Just inp) ->
        pipeInput st puppetIdx (Just inp)
          =<< pipeInput st puppetIdx Nothing (cmdOutPSt :!: mSyncCwdPSt)
  pure
    ( st & mst_puppetSt . pupIdx puppetIdx .~ (newThisPup & ps_outputParser .~ newCmdOutP)
        & mst_syncCwdP .~ ((^. _2) <$> newMuxProg)
    )

whenJustC :: Maybe (a -> a) -> a -> a
whenJustC Nothing cont = cont
whenJustC (Just act) cont = act cont

switchPuppets :: MuxEnv -> MuxState -> IO (Maybe MuxState)
switchPuppets env st0 = do
  let fromIdx = st0 ^. mst_currentPuppetIdx
  let toIdx = nextPuppet (st0 ^. mst_currentPuppetIdx)
  let st = st0 {_mst_currentPuppetIdx = toIdx}
  let (toSt :!: fromSt) = st ^. mst_sortedPuppets
  let (toPup :!: fromPup) = env ^. menv_sortedPuppets st

  (toProc, startedNewProc, newSt) <-
    case _ps_process toSt of
      Just pid -> pure (pid, False, st)
      Nothing -> do
        Protolude.putStrLn ("\r\nStarting " <> show (_pup_cmd toPup) <> " ..\r\n" :: Text)
        pid <- _pup_startProcess toPup
        pure (pid, True, st & mst_currentPuppet . ps_process ?~ pid)

  let copyPrevCmdC = liftP_ (copyToXClipboard . unStrippedCmdResult . stripCmdOut $ _ps_prevCmdOut fromSt)
      selectInp idx (inpIdx, x) = if idx == inpIdx then Just x else Nothing
      adapt idx p = Adapter (selectInp idx) (idx,) p
      clearPromptToC = AndThen (adapt toIdx $ _pup_cleanPromptP toPup toProc)

  {- ORMOLU_DISABLE -}
  let mSyncCwdC =
        case _ps_process fromSt of
          Nothing -> Nothing
          Just fromProc ->
            Just
              ( \cont ->
                  AndThen (adapt fromIdx $ _pup_cleanPromptP fromPup fromProc) $
                  syncCwdC (_pp_pid toProc :!: _pp_pid fromProc) env toIdx $
                  cont
              )
      program =
        liftP_
          ( do hPutStrLn stderr "~ Switch puppets program started"
               _pup_switchExitHook toPup
               _pup_switchEnterHook toPup
           ) $
        ( \cont ->
            case (_ps_mode fromSt, _ps_mode toSt) of
              (fromMode, PuppetModeTUI) ->
                unlessC startedNewProc
                  ( liftP_ $ do
                      when (fromMode == PuppetModeRepl)
                        (BS.hPut stdout "\ESC[?1049h")   -- enable alternative screen buffer ("TUI" mode)
                      jiggleTtySize (_pp_pts toProc)     -- a hack to force a TUI app to redraw it's interface
                   )
                cont
              (fromMode, PuppetModeRepl) ->
                whenC (fromMode == PuppetModeTUI)
                  ( liftP_ -- clear tui interface
                      (do BS.hPut stdout "\ESC[?1049l" -- disable alternative screen buffer (disable "TUI" mode)
                          showCursor
                       )
                   ) $
                -- clear the current line and until the end of screen
                liftP_ (BS.hPut stdout "\ESC[J\ESC[2K\ESC[A") $
                unlessC startedNewProc clearPromptToC $
                whenJustC mSyncCwdC
                cont
          ) $
        copyPrevCmdC $
        liftP_ (hPutStrLn stderr "~ Switch puppets program finished")
        finishP
  {- ORMOLU_ENABLE -}
  Just <$> runMuxPrograms (newSt & mst_syncCwdP ?~ program) toIdx Nothing

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
