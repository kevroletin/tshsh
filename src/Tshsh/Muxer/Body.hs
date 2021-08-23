{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Tshsh.Muxer.Body where

import Control.Exception.Safe (tryIO)
import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import Data.Strict.Tuple.Extended
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Foreign hiding (void)
import Protolude hiding (hPutStrLn, log, tryIO)
import System.Console.ANSI
import System.IO hiding (hPutStr)
import System.IO.Temp
import System.Process
import Tshsh.Commands
import Tshsh.Data.BufferSlice (BufferSlice (..), SliceList (..))
import qualified Tshsh.Data.BufferSlice as BufferSlice
import Tshsh.KeyParser
import Tshsh.Lang.Coroutine.CPS
import Tshsh.Muxer.Log
import Tshsh.Muxer.SyncCwd
import Tshsh.Muxer.Types
import Tshsh.Puppet
import Tshsh.Tty

copyToXClipboard :: Text -> IO ()
copyToXClipboard str =
  unless (T.all isSpace str) $ do
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

onSyncCwdOut :: MuxState -> (PuppetIdx, ByteString) -> IO ()
onSyncCwdOut st (i, x) = do
  hPutStr stderr $ "~> " <> (show (i, x) :: Text) <> "\n"
  case st ^. mst_puppetSt . pupIdx i . ps_process of
    Just p -> BS.hPut (_pp_inputH p) x
    Nothing -> pure ()

-- manually loop over output of commands output parser and feed into sync cwd
runMuxPrograms_ ::
  MuxState ->
  PuppetIdx ->
  BufferSlice ->
  ( StrippedCmdResult,
    ProgramEvSt PuppetState BufferSlice StrippedCmdResult IO,
    Maybe (ProgramEv 'Ev () (PuppetIdx, StrippedCmdResult) (PuppetIdx, ByteString) IO)
  ) ->
  IO
    ( StrippedCmdResult,
      ProgramEvSt PuppetState BufferSlice StrippedCmdResult IO,
      Maybe (ProgramEv 'Ev () (PuppetIdx, StrippedCmdResult) (PuppetIdx, ByteString) IO)
    )
runMuxPrograms_ st puppetIdx i (prevCmdOut0, producer0, mConsumer0) =
  loop prevCmdOut0 mConsumer0 =<< stepInput i producer0
  where
    loop prevCmdOut mConsumer = \case
      ResOut (_ :!: r) -> do
        throwIO . FatalError $ "Input parser " <> show puppetIdx <> " terminated with: " <> show r
      ContNoOut prodCont ->
        pure (prevCmdOut, prodCont, mConsumer)
      ContOut newCmdOut prodCont -> do
        case mConsumer of
          Nothing ->
            -- remember newCmdOut only if SyncCwd is not running
            loop newCmdOut Nothing =<< stepOut prodCont
          Just consumer ->
            feedInputM (onSyncCwdOut st) (puppetIdx, newCmdOut) (() :!: consumer) >>= \case
              Cont (() :!: consCont) -> do
                loop prevCmdOut0 (Just consCont) =<< stepOut prodCont
              Res r -> do
                hPutStrLn stderr $ "Sync cwd terminated with: " <> show r
                loop prevCmdOut0 Nothing =<< stepOut prodCont

runMuxPrograms :: MuxState -> PuppetIdx -> BufferSlice -> IO MuxState
runMuxPrograms st puppetIdx inp = do
  let thisPuppet = st ^. mst_puppetSt . pupIdx puppetIdx
      cmdOutPSt = thisPuppet :!: thisPuppet ^. ps_outputParser
  (prevCmdOut, (newThisPup :!: newCmdOutP), newMuxProg) <-
    runMuxPrograms_ st puppetIdx inp (_mst_prevCmdOut st, cmdOutPSt, _mst_syncCwdP st)
  pure
    ( st & mst_puppetSt . pupIdx puppetIdx .~ (newThisPup & ps_outputParser .~ newCmdOutP)
        & mst_syncCwdP .~ newMuxProg
        & mst_prevCmdOut .~ prevCmdOut
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

  let selectInp idx (inpIdx, x) = if idx == inpIdx then Just x else Nothing
      adapt idx p = Adapter (selectInp idx) (idx,) p
      clearPromptToC = AndThen (adapt toIdx $ _pup_cleanPromptP toPup toProc)

  {- ORMOLU_DISABLE -}
  let mSyncCwdC =
        case _ps_process fromSt of
          Nothing -> Nothing
          Just fromProc ->
            Just
              ( \cont ->
                  whenC (_ps_mode fromSt == PuppetModeRepl)
                    (AndThen (adapt fromIdx $ _pup_cleanPromptP fromPup fromProc)) $
                  syncCwdC (_pp_pid toProc :!: _pp_pid fromProc) env toIdx $
                  cont
              )
      program =
        liftP_
          ( do hPutStrLn stderr "~ Switch puppets program started"
               _pup_switchExitHook fromPup
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
                -- clear the current line and until the end of screen, go to up
                liftP_ (BS.hPut stdout "\ESC[J\ESC[2K\ESC[A") $
                (if startedNewProc
                   then (WaitInput . const)
                   else clearPromptToC) $
                whenJustC mSyncCwdC
                cont
          ) $
        liftP_ (hPutStrLn stderr "~ Switch puppets program finished")
        finishP
  {- ORMOLU_ENABLE -}

  mProgram <-
    eatOutputsM (onSyncCwdOut newSt) (() :!: program) >>= \case
      Cont (_ :!: consCont) ->
        pure (Just consCont)
      Res r -> do
        hPutStrLn stderr $ "Sync cwd terminated with: " <> show r
        pure Nothing

  pure $ Just (newSt & mst_syncCwdP .~ mProgram)

muxOnTermInput :: MuxState -> ByteString -> IO (Maybe MuxState)
muxOnTermInput st str = do
  muxLog (st ^. mst_currentPuppetIdx, str)
  case st ^. mst_currentPuppet . ps_process of
    Nothing -> pure ()
    Just p -> BS.hPut (_pp_inputH p) str
  pure (Just st)

muxOnKeyBinding :: MuxEnv -> MuxState -> MuxKeyCommands -> IO (Maybe MuxState)
muxOnKeyBinding env st key = do
  hPutStrLn stderr ("Key< " <> show key)
  case key of
    MuxKeySwitch ->
      switchPuppets env st
    MuxKeyCopyLastOut -> do
      copyToXClipboard . unStrippedCmdResult $ _mst_prevCmdOut st
      pure (Just st)
    MuxKeyEditLastOut -> do
      let prevOut = unStrippedCmdResult (_mst_prevCmdOut st)
      unless (T.all isSpace prevOut) $ do
        hPutStrLn stderr "Os < emacsclient"
        void . tryIO $ do
          fname <- emptySystemTempFile "tshsh_cmdout"
          T.writeFile fname prevOut
          void $ spawnProcess "emacsclient" ["-n", "-c", fname]
      pure (Just st)

muxBody :: MuxEnv -> MuxState -> MuxCmd -> IO (Maybe MuxState)
muxBody env st0 (TermInput bs) = do
  let str0 = BufferSlice.sliceToByteString bs

      loop _ Nothing = pure Nothing
      loop res (Just st) =
        case res of
          KeyParserData out next ->
            loop next =<< muxOnTermInput st out
          KeyParserAction act next -> do
            loop next =<< muxOnKeyBinding env st act
          KeyParserNull next ->
            pure $ Just (next, st)
  loop (keyParserRun (_mst_inputParser st0) str0) (Just st0) >>= \case
    Nothing ->
      pure Nothing
    Just (newKeyParser, newSt) ->
      pure $ Just (newSt {_mst_inputParser = newKeyParser})
muxBody _env st (PuppetOutput puppetIdx inp@(BufferSlice _ buf size)) = do
  when (puppetIdx == st ^. mst_currentPuppetIdx) $
    withForeignPtr buf $ \ptr -> do
      hPutBuf stdout ptr size
  Just <$> runMuxPrograms st puppetIdx inp
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
