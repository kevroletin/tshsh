{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Tshsh.Muxer.Handlers
  ( onTermInput,
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
import qualified Data.Map.Strict as Map
import Data.Strict.Tuple.Extended
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
    ProgramEvSt OutputParserSt BufferSlice StrippedCmdResult IO,
    Maybe (ProgramEv 'Ev () (PuppetIdx, StrippedCmdResult) (PuppetIdx, ByteString) IO)
  ) ->
  IO
    ( StrippedCmdResult,
      ProgramEvSt OutputParserSt BufferSlice StrippedCmdResult IO,
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
                hPutStrLn stderr $ ("Sync cwd terminated with: " <> show r :: Text)
                loop prevCmdOut0 Nothing =<< stepOut prodCont

runMuxPrograms :: MuxState -> PuppetIdx -> BufferSlice -> IO MuxState
runMuxPrograms st puppetIdx inp = do
  case st ^? mst_puppets . ix puppetIdx . ps_outputParser of
    Nothing -> pure st
    Just cmdOutPSt -> do
      (prevCmdOut, newCmdOutP, newMuxProg) <-
        runMuxPrograms_ st puppetIdx inp (_mst_prevCmdOut st, cmdOutPSt, _mst_syncCwdP st)
      pure
        ( st & mst_puppets . ix puppetIdx . ps_outputParser .~ newCmdOutP
            & mst_syncCwdP .~ newMuxProg
            & mst_prevCmdOut .~ prevCmdOut
        )

switchPuppetsTo :: MuxEnv -> MuxState -> PuppetIdx -> Maybe PuppetMode -> IO (Maybe MuxState)
switchPuppetsTo env st0 toIdx prevMode = do
  let (Just fromMode) =
        (st0 ^? mst_currentPuppet . _Just . ps_mode)
          <|> prevMode
  let fromIdx = st0 ^. mst_currentPuppetIdx
  let st = st0 {_mst_currentPuppetIdx = toIdx, _mst_prevPuppetIdx = fromIdx}
  let mToSt = st ^. mst_currentPuppet
  let mFromSt = st ^. mst_prevPuppet
  let fromPup = (env ^? menv_puppets . ix fromIdx) & fromMaybe (panic "fromIdx is out of bounds")
  let toPup = (env ^? menv_puppets . ix toIdx) & fromMaybe (panic "toIdx is out of bounds")

  (startedNewProc, toSt, newSt) <-
    case mToSt of
      Just x -> pure (False, x, st)
      Nothing -> do
        Protolude.putStrLn ("\r\nStarting " <> show (_pc_cmd toPup) <> " ..\r\n" :: Text)
        newSt <- startPuppetProcess (_menv_outputAvailable env) toIdx toPup
        pure (True, newSt, st & mst_currentPuppet ?~ newSt)

  let clearPromptHookC pupSt = AndThen (adaptPuppetAct pupSt $ (pupSt ^. ps_cfg . pc_cleanPromptP) (_ps_process pupSt))

  {- ORMOLU_DISABLE -}
  let mSyncCwdC =
        case mFromSt of
          Nothing -> (\x -> x)
          Just fromSt ->
            ( \cont ->
                clearPromptHookC fromSt $
                syncCwdC toSt fromSt $
                cont
            )
      runSwitchHooks =
        liftP_
          ( do (fromPup ^. pc_switchExitHook)
               (toPup ^.  pc_switchEnterHook)
           )
      restoreTermStateC cont =
        case toSt ^. ps_mode of
          PuppetModeTUI ->
            unlessC startedNewProc
              ( liftP_ $ do
                  when (fromMode == PuppetModeRepl)
                    (BS.hPut stdout "\ESC[?1049h")   -- enable alternative screen buffer ("TUI" mode)
                  jiggleTtySize (toSt ^. ps_process . pp_pts)     -- a hack to force a TUI app to redraw it's interface
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
      clearPromptToC cont =
        case toSt ^. ps_mode of
          PuppetModeTUI ->
            -- the problem here is that we ignore output of TUI apps and hence
            -- we unable to write any meaningful condition to wait until an app
            -- has started and became ready to receive interactive commands;
            -- TODO: we wait here to cd, we can just start a process in a right
            -- directory; but that adds complexity of starting a puppet from
            -- Program
            (if startedNewProc
                -- TODO: oh oh
                then (Lift (threadDelay 100000) . const)
                else clearPromptHookC toSt) $
            cont
          PuppetModeRepl ->
            (if startedNewProc
                then (WaitInput . const)
                else clearPromptHookC toSt) $
            cont
      program =
        liftP_ (hPutStrLn stderr ("~ Switch puppets program started" :: Text)) $
        runSwitchHooks $
        restoreTermStateC $
        clearPromptToC $
        mSyncCwdC $
        liftP_ (hPutStrLn stderr ("~ Switch puppets program finished" :: Text))
        finishP
  {- ORMOLU_ENABLE -}

  if (isGetCwdNoSupport $ _pc_getCwdCmd toPup) || (isCdNoSupport $ _pc_cdCmd toPup)
    then pure $ Just (newSt & mst_syncCwdP .~ Nothing)
    else do
      mProgram <-
        eatOutputsM (onSyncCwdOut newSt) (() :!: program) >>= \case
          Cont (_ :!: consCont) ->
            pure (Just consCont)
          Res r -> do
            hPutStrLn stderr $ ("Sync cwd terminated with: " <> show r :: Text)
            pure Nothing

      pure $ Just (newSt & mst_syncCwdP .~ mProgram)

onTermInput :: MuxState -> ByteString -> IO (Maybe MuxState)
onTermInput st str = do
  muxLog (st ^. mst_currentPuppetIdx, str)
  case st ^? mst_currentPuppet . _Just . ps_process of
    Nothing -> pure ()
    Just p -> BS.hPut (_pp_inputH p) str
  pure (Just st)

onKeyBinding :: MuxEnv -> MuxState -> MuxKeyCommands -> IO (Maybe MuxState)
onKeyBinding env st key = do
  hPutStrLn stderr ("Key< " <> show key :: Text)
  case key of
    MuxKeySwitch -> do
      switchPuppetsTo env st (st ^. mst_prevPuppetIdx) Nothing
    MuxKeyCopyLastOut -> do
      copyToXClipboard . unStrippedCmdResult $ _mst_prevCmdOut st
      pure (Just st)
    MuxKeyEditLastOut -> do
      let prevOut = unStrippedCmdResult (_mst_prevCmdOut st)
      unless (T.all isSpace prevOut) $ do
        hPutStrLn stderr ("Os < emacsclient" :: Text)
        void . tryIO $ do
          fname <- emptySystemTempFile "tshsh_cmdout"
          T.writeFile fname prevOut
          void $ spawnProcess "emacsclient" ["-n", "-c", fname]
      pure (Just st)
    MuxKeySwitchPuppet newIdx ->
      switchPuppetsTo env st newIdx Nothing

onPuppetOutput :: MuxEnv -> MuxState -> PuppetIdx -> BufferSlice -> IO (Maybe MuxState)
onPuppetOutput _env st puppetIdx inp@(BufferSlice _ buf size) = do
  when (puppetIdx == st ^. mst_currentPuppetIdx) $
    withForeignPtr buf $ \ptr -> do
      hPutBuf stdout ptr size
  Just <$> runMuxPrograms st puppetIdx inp

onSwitchPuppets :: MuxEnv -> MuxState -> IO (Maybe MuxState)
onSwitchPuppets env st0 = do
  switchPuppetsTo env st0 (st0 ^. mst_prevPuppetIdx) Nothing

onSignal :: MuxEnv -> MuxState -> MuxSignal -> IO (Maybe MuxState)
onSignal _env st WindowResize = do
  traverse_ syncTtySize (st ^? mst_currentPuppet . _Just . ps_process . pp_pts)
  traverse_ syncTtySize (st ^? mst_prevPuppet . _Just . ps_process . pp_pts)
  pure (Just st)
onSignal env st0 (ChildExited exitedPid) = do
  case st0 ^. mst_currentPuppet of
    Nothing -> pure Nothing
    Just currSt ->
      if currSt ^. ps_process . pp_pid == exitedPid
        then do
          let newPuppets = Map.delete (_ps_idx currSt) (_mst_puppets st0)
          let newSt =
                ( st0 & mst_currentPuppet .~ Nothing
                    & mst_puppets .~ newPuppets
                    & mst_syncCwdP .~ Nothing
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
              ( if exitedIdx == _mst_prevPuppetIdx st0
                  then st0 & mst_syncCwdP .~ Nothing
                  else st0
              )
                & mst_puppets %~ (Map.delete exitedIdx)
