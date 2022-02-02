{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Tshsh.Muxer.Handlers
  ( onTermInput,
    onProgTimeout,
    onKeyBinding,
    onPuppetOutput,
    onSwitchPuppets,
    onSignal,
  )
where

import Control.Exception.Safe (tryIO)
import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as Map
import Data.Strict.Tuple.Extended
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (getCurrentTime)
import Foreign hiding (void)
import Protolude hiding (log, tryIO)
import System.Console.ANSI
import System.IO (hClose, hPutBuf)
import System.IO.Temp
import System.Process
import Tshsh.Commands
import Tshsh.Data.BufferSlice (BufferSlice (..))
import Tshsh.Lang.Coroutine.CPS
import Tshsh.Muxer.Log
import Tshsh.Muxer.PuppetProcess
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

onSyncCwdOut :: MuxState -> (PuppetIdx, ByteString) -> IO ()
onSyncCwdOut st (i, x) = do
  hPutStr stderr $ "~> " <> (show (i, x) :: Text) <> "\n"
  case st ^? mst_puppets . ix i . ps_process of
    Just p -> BS.hPut (_pp_inputH p) x
    Nothing -> pure ()

-- manually loop over output of commands output parser and feed into sync cwd
runMuxPrograms_ ::
  MuxState ->
  PuppetIdx ->
  BufferSlice ->
  ( StrippedCmdResult,
    ProgramEvSt OutputParserSt BufferSlice StrippedCmdResult IO (),
    Maybe (ProgramEv 'Ev MuxState (PuppetIdx, StrippedCmdResult) (PuppetIdx, ByteString) IO ())
  ) ->
  IO
    ( MuxState,
      StrippedCmdResult,
      ProgramEvSt OutputParserSt BufferSlice StrippedCmdResult IO (),
      Maybe (ProgramEv 'Ev MuxState (PuppetIdx, StrippedCmdResult) (PuppetIdx, ByteString) IO ())
    )
runMuxPrograms_ st0 puppetIdx inp (prevCmdOut0, producer0, mConsumer0) = do
  env <- StepEnv <$> getCurrentTime
  loop env st0 prevCmdOut0 mConsumer0 =<< stepInput env inp producer0
  where
    loop env !st prevCmdOut mConsumer = \case
      ResOut (_ :!: r) -> do
        throwIO . FatalError $ "Input parser " <> show puppetIdx <> " terminated with: " <> show r
      ContNoOut prodCont ->
        pure (st, prevCmdOut, prodCont, mConsumer)
      ContOut newCmdOut prodCont -> do
        case mConsumer of
          Nothing -> do
            -- remember newCmdOut only if SyncCwd is not running and if output is not empty
            let newCmdOut1 =
                  if C8.all isSpace (unStrippedCmdResult newCmdOut)
                    then prevCmdOut0
                    else newCmdOut
            loop env st newCmdOut1 Nothing =<< stepOut env prodCont
          Just consumer ->
            feedInputM env (onSyncCwdOut st) (puppetIdx, newCmdOut) (st :!: consumer) >>= \case
              Cont (newSt :!: consCont) -> do
                loop env newSt prevCmdOut0 (Just consCont) =<< stepOut env prodCont
              Res (newSt :!: res) -> do
                hPutStrLn stderr $ ("Sync cwd terminated with: " <> show res :: Text)
                loop env newSt prevCmdOut0 Nothing =<< stepOut env prodCont

runMuxPrograms :: MuxState -> PuppetIdx -> BufferSlice -> IO MuxState
runMuxPrograms st puppetIdx inp = do
  case st ^? mst_puppets . ix puppetIdx . ps_outputParser of
    Nothing -> pure st
    Just cmdOutPSt -> do
      (newSt, prevCmdOut, newCmdOutP, newMuxProg) <-
        runMuxPrograms_ st puppetIdx inp (_mst_prevCmdOut st, cmdOutPSt, _mst_switchPupP st)
      pure
        ( newSt & mst_puppets . ix puppetIdx . ps_outputParser .~ newCmdOutP
            & mst_switchPupP .~ newMuxProg
            & mst_prevCmdOut .~ prevCmdOut
        )

switchPuppetsTo :: MuxEnv -> MuxState -> PuppetIdx -> Maybe PuppetMode -> IO (Maybe MuxState)
switchPuppetsTo env st0 toIdx prevMode
  | st0 ^. mst_currentPuppetIdx == toIdx =
    if st0 ^. mst_currentPuppetIdx == st0 ^. mst_prevPuppetIdx
      then pure (Just st0)
      else switchPuppetsTo env st0 (st0 ^. mst_prevPuppetIdx) Nothing
  | otherwise = do
    let (Just fromMode) =
          (st0 ^? mst_currentPuppet . _Just . ps_mode)
            <|> prevMode
    let fromIdx = st0 ^. mst_currentPuppetIdx
    let st = st0 {_mst_currentPuppetIdx = toIdx, _mst_prevPuppetIdx = fromIdx}
    let mFromSt = st ^. mst_prevPuppet
    let fromPup = (env ^? menv_puppets . ix fromIdx) & fromMaybe (panic "fromIdx is out of bounds")
    let toPup = (env ^? menv_puppets . ix toIdx) & fromMaybe (panic "toIdx is out of bounds")

    {- ORMOLU_DISABLE -}
    let startNewProc = isNothing (st ^. mst_puppets . at toIdx)

        startPupC mCwd mEnv cont =
          case st ^. mst_puppets . at toIdx of
            Just x ->
              cont x
            Nothing ->
              Lift ( do
                Protolude.putStrLn ("\r\nStarting " <> show (_pc_cmd toPup) <> " ..\r\n" :: Text)
                startPuppetProcess mCwd mEnv (_menv_outputAvailable env) toIdx toPup
              ) $ \newPupSt ->
              ModifyState (mst_puppets . at toIdx ?~ newPupSt) $
              cont newPupSt

    let clearPromptHookC pupSt = andThenP_ (adaptPuppetAct pupSt $ (pupSt ^. ps_cfg . pc_cleanPromptP) (_ps_process pupSt))

    let mGetCwdC cont =
          case mFromSt of
            Nothing -> cont Nothing
            Just fromSt ->
              getPuppetCwd fromSt cont
        mGetEnvC cont =
          case mFromSt of
            Nothing -> cont Nothing
            Just fromSt ->
              case fromSt ^. ps_cfg . pc_getEnvCmd of
                GetEnvNoSupport -> cont Nothing
                GetEnvFromProcess ->
                  Lift
                    ( do str <- readFile ("/proc/" <> show (fromSt ^. ps_process . pp_pid) <> "/environ")
                         pure . fmap (bimap (\x -> x) (T.strip . T.drop 1) . T.breakOn "=") . T.split (=='\x0') $ str
                     )
                  (cont . Just)
                GetEnvProgram p ->
                  AndThen (adaptPuppetAct fromSt (p (fromSt ^. ps_process))) (cont . Just)
        runSwitchHooks =
          liftP_
            ( do (fromPup ^. pc_switchExitHook)
                 (toPup ^.  pc_switchEnterHook)
            )
        restoreTermStateC toSt cont =
          case toSt ^. ps_mode of
            PuppetModeTUI ->
              -- enable alternative screen buffer ("TUI" mode)
              liftP_ (when (fromMode == PuppetModeRepl) (BS.hPut stdout "\ESC[?1049h")) $
              unlessC startNewProc
                ( case toSt ^. ps_cfg . pc_refreshTui of
                    RefreshTuiJiggleTty ->
                      liftP_ (jiggleTtySize (toSt ^. ps_process . pp_pts))
                    RefreshTuiSendOutput str ->
                      Output (toSt ^. ps_idx, str)
                )
              cont
            PuppetModeRepl ->
              whenC (fromMode == PuppetModeTUI)
                ( liftP_ -- clear tui interface
                    (do BS.hPut stdout "\ESC[?1049l" -- disable alternative screen buffer (disable "TUI" mode)
                        showCursor
                      )
                  ) $
              -- clear the current line and until the end of screen, go to up
              liftP_ (BS.hPut stdout "\ESC[J\ESC[2K\ESC[A") $
              cont
        clearPromptToC toSt cont =
          case toSt ^. ps_mode of
            PuppetModeTUI ->
              -- the problem here is that we ignore output of TUI apps and hence
              -- we unable to write any meaningful condition to wait until an app
              -- has started and became ready to receive interactive commands; so
              -- trying to run interactive commands in a TUI app after start will
              -- require adding a magic delay here
              if startNewProc
                then cont
                else clearPromptHookC toSt cont
            PuppetModeRepl ->
              if startNewProc
                then $waitInputInfC (const cont)
                else clearPromptHookC toSt cont
        program =
          liftP_ (hPutStrLn stderr ("~ Switch puppets program started" :: Text)) $
          (ifC startNewProc mGetEnvC ($ Nothing)) $ \mEnv ->
          mGetCwdC $ \mCwd ->
          startPupC mCwd mEnv $ \toSt ->
          adaptUnitStP (
            runSwitchHooks $
            restoreTermStateC toSt $
            clearPromptToC toSt $
            unlessC startNewProc
              (puppetCdC toSt mCwd) $
            liftP_ (hPutStrLn stderr ("~ Switch puppets program finished" :: Text))
            finishP_
          )
    {- ORMOLU_ENABLE -}

    stepEnv <- StepEnv <$> getCurrentTime
    (newSt, mProgram) <-
      eatOutputsM stepEnv (onSyncCwdOut st) (st :!: program) >>= \case
        Cont (newSt :!: cont) ->
          pure (newSt, Just cont)
        Res (newSt :!: r) -> do
          hPutStrLn stderr $ ("Sync cwd terminated with: " <> show r :: Text)
          pure (newSt, Nothing)

    pure $ Just (newSt & mst_switchPupP .~ mProgram)

onTermInput :: MuxState -> ByteString -> IO (Maybe MuxState)
onTermInput st str = do
  muxLog ("onTermInput" :: Text, str)
  case st ^? mst_currentPuppet . _Just . ps_process of
    Nothing -> pure ()
    Just p -> BS.hPut (_pp_inputH p) str
  pure (Just st)

onProgTimeout :: MuxState -> IO (Maybe MuxState)
onProgTimeout st = do
  muxLog ("onProgTimeout" :: Text)
  case _mst_switchPupP st of
    Nothing -> pure (Just st)
    Just switchPupP -> do
      stepEnv <- StepEnv <$> getCurrentTime
      eatOutputsM stepEnv (onSyncCwdOut st) (st :!: switchPupP) >>= \case
        Cont (newSt :!: newSwitchPupP) -> do
          pure (Just newSt {_mst_switchPupP = Just newSwitchPupP})
        Res (newSt :!: res) -> do
          hPutStrLn stderr $ ("Sync cwd terminated with: " <> show res :: Text)
          pure (Just newSt {_mst_switchPupP = Nothing})

onKeyBinding :: MuxEnv -> MuxState -> MuxKeyCommands -> IO (Maybe MuxState)
onKeyBinding env st key = do
  muxLog ("onKeyBinding" :: Text, key)
  hPutStrLn stderr ("Key< " <> show key :: Text)
  case key of
    MuxKeySwitch -> do
      switchPuppetsTo env st (st ^. mst_prevPuppetIdx) Nothing
    MuxKeyCopyLastOut -> do
      copyToXClipboard . decodeUtf8 . unStrippedCmdResult $ _mst_prevCmdOut st
      pure (Just st)
    MuxKeyPasteLastOut -> do
      let str = unStrippedCmdResult $ _mst_prevCmdOut st
      onTermInput st str
    MuxKeyEditLastOut -> do
      let prevOut = unStrippedCmdResult (_mst_prevCmdOut st)
      unless (C8.all isSpace prevOut) $ do
        hPutStrLn stderr ("Os < emacsclient" :: Text)
        void . tryIO $ do
          fname <- emptySystemTempFile "tshsh_cmdout"
          BS.writeFile fname prevOut
          void $ spawnProcess "emacsclient" ["-n", "-c", fname]
      pure (Just st)
    MuxKeySwitchPuppet newIdx ->
      switchPuppetsTo env st newIdx Nothing

onPuppetOutput :: MuxState -> PuppetIdx -> BufferSlice -> IO (Maybe MuxState)
onPuppetOutput st puppetIdx inp@(BufferSlice _ buf size) = do
  muxLog ("onPuppetOutput" :: Text, puppetIdx, inp)
  when (puppetIdx == st ^. mst_currentPuppetIdx) $
    withForeignPtr buf $ \ptr -> do
      hPutBuf stdout ptr size
  Just <$> runMuxPrograms st puppetIdx inp

onSwitchPuppets :: MuxEnv -> MuxState -> IO (Maybe MuxState)
onSwitchPuppets env st0 = do
  muxLog ("onSwitchPuppets" :: Text)
  switchPuppetsTo env st0 (st0 ^. mst_prevPuppetIdx) Nothing

onSignal :: MuxEnv -> MuxState -> MuxSignal -> IO (Maybe MuxState)
onSignal _env st WindowResize = do
  muxLog ("onSignal" :: Text, WindowResize)
  traverse_ syncTtySize (st ^? mst_currentPuppet . _Just . ps_process . pp_pts)
  traverse_ syncTtySize (st ^? mst_prevPuppet . _Just . ps_process . pp_pts)
  pure (Just st)
onSignal env st0 (ChildExited exitedPid) = do
  muxLog ("onSignal" :: Text, (ChildExited exitedPid))
  case st0 ^. mst_currentPuppet of
    Nothing -> pure Nothing
    Just currSt ->
      if currSt ^. ps_process . pp_pid == exitedPid
        then do
          let newPuppets = Map.delete (_ps_idx currSt) (_mst_puppets st0)
          let newSt =
                ( st0 & mst_currentPuppet .~ Nothing
                    & mst_puppets .~ newPuppets
                )
          if not (Map.null newPuppets)
            then
              let toIdx =
                    case Map.lookup (_mst_prevPuppetIdx st0) newPuppets of
                      Nothing -> let (x, _) = Map.findMin newPuppets in x
                      Just _ -> _mst_prevPuppetIdx st0
               in switchPuppetsTo env newSt toIdx (Just $ currSt ^. ps_mode)
            else
              if _mst_keepAlive st0
                then switchPuppetsTo env newSt (_menv_defaultPuppet env) (Just $ currSt ^. ps_mode)
                else pure Nothing
        else case find (\(_, ps) -> ps ^. ps_process . pp_pid == exitedPid) $ Map.toList (_mst_puppets st0) of
          Nothing ->
            pure (Just st0)
          Just (exitedIdx, _) ->
            pure . Just $
              st0 & mst_puppets %~ (Map.delete exitedIdx)
