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
import Tshsh.Muxer.ShellOutputParser
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
pipeInput ::
  MuxState ->
  PuppetIdx ->
  inp ->
  Pair
    (ProgramEvSt st1 inp RawCmdResult IO)
    (Maybe (ProgramEvSt () (PuppetIdx, StrippedCmdResult) (PuppetIdx, ByteString) IO)) ->
  IO
    ( Pair
        (ProgramEvSt st1 inp RawCmdResult IO)
        (Maybe (ProgramEvSt () (PuppetIdx, StrippedCmdResult) (PuppetIdx, ByteString) IO))
    )
pipeInput st puppetIdx i (producer :!: mConsumer0) =
  loop mConsumer0 =<< stepInput i producer
  where
    loop mConsumer = \case
      ResOut (_ :!: r) -> panic ("oops, input parser exited with " <> show r)
      ContNoOut prodCont ->
        pure (prodCont :!: mConsumer)
      ContOut o prodCont -> do
        logSliceList (show puppetIdx <> "> ") 40 (unRawCmdResult o)
        case mConsumer of
          Nothing -> loop Nothing =<< stepOut prodCont
          Just consumer ->
            feedInputM (onSyncCwdOut st) (puppetIdx, stripCmdOut o) consumer >>= \case
              Cont consCont ->
                loop (Just consCont) =<< stepOut prodCont
              Res r -> do
                hPutStrLn stderr $ "Sync cwd terminated with: " <> show r
                loop Nothing =<< stepOut prodCont

runMuxPrograms :: MuxState -> PuppetIdx -> BufferSlice -> IO MuxState
runMuxPrograms st puppetIdx inp = do
  let thisPuppet = st ^. mst_puppetSt . pupIdx puppetIdx
      cmdOutPSt = thisPuppet :!: thisPuppet ^. ps_outputParser
      mSyncCwdPSt = (() :!:) <$> _mst_syncCwdP st
  ((newThisPup :!: newCmdOutP) :!: newMuxProg) <-
    pipeInput st puppetIdx inp (cmdOutPSt :!: mSyncCwdPSt)
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
                unlessC startedNewProc clearPromptToC $
                whenJustC mSyncCwdC
                cont
          ) $
        copyPrevCmdC $
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
      let currPup = st ^. mst_currentPuppet
      copyToXClipboard . unStrippedCmdResult . stripCmdOut $ _ps_prevCmdOut currPup
      pure (Just st)
    MuxKeyEditLastOut -> do
      -- TODO: we will copy a wrong command after switch cause a correct previous command was from
      -- a previous puppet
      let currPup = st ^. mst_currentPuppet
      let prevOut = unStrippedCmdResult . stripCmdOut $ _ps_prevCmdOut currPup
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
