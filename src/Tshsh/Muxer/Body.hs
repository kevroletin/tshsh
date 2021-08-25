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
import qualified Data.Map.Strict as Map
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
                hPutStrLn stderr $ "Sync cwd terminated with: " <> show r
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

whenJustC :: Maybe (a -> a) -> a -> a
whenJustC Nothing cont = cont
whenJustC (Just act) cont = act cont

switchPuppets :: MuxEnv -> MuxState -> Maybe PuppetMode -> IO (Maybe MuxState)
switchPuppets env st0 prevMode = do
  let (Just fromMode) =
        (st0 ^? mst_currentPuppet . _Just . ps_mode)
          <|> prevMode
  let fromIdx = st0 ^. mst_currentPuppetIdx
  let toIdx = st0 ^. mst_prevPuppetIdx
  let st = st0 {_mst_currentPuppetIdx = toIdx, _mst_prevPuppetIdx = fromIdx}
  let mToSt = st ^. mst_currentPuppet
  let mFromSt = st ^. mst_prevPuppet
  let fromPup = (env ^? menv_puppets . ix fromIdx) & fromMaybe (panic "fromIdx is out of bounds")
  let toPup = (env ^? menv_puppets . ix toIdx) & fromMaybe (panic "toIdx is out of bounds")

  (startedNewProc, toSt, newSt) <-
    case mToSt of
      Just x -> pure (False, x, st)
      Nothing -> do
        Protolude.putStrLn ("\r\nStarting " <> show (_pup_cmd toPup) <> " ..\r\n" :: Text)
        newSt <- _pup_startProcess toPup
        pure (True, newSt, st & mst_currentPuppet ?~ newSt)

  let selectInp idx (inpIdx, x) = if idx == inpIdx then Just x else Nothing
      adapt idx p = Adapter (selectInp idx) (idx,) p
      clearPromptToC = AndThen (adapt toIdx $ (toSt ^. ps_cfg . pc_cleanPromptP) (_ps_process toSt))

  {- ORMOLU_DISABLE -}
  let mSyncCwdC =
        case mFromSt of
          Nothing -> Nothing
          Just fromSt ->
            Just
              ( \cont ->
                  whenC (fromSt ^. ps_mode == PuppetModeRepl)
                    (AndThen (adapt fromIdx $ (fromSt ^. ps_cfg . pc_cleanPromptP) (_ps_process fromSt))) $
                  syncCwdC toSt fromSt $
                  cont
              )
      program =
        liftP_
          ( do hPutStrLn stderr "~ Switch puppets program started"
               (fromPup ^. pup_cfg . pc_switchExitHook)
               (toPup ^. pup_cfg . pc_switchEnterHook)
           ) $
        ( \cont ->
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
  case st ^? mst_currentPuppet . _Just . ps_process of
    Nothing -> pure ()
    Just p -> BS.hPut (_pp_inputH p) str
  pure (Just st)

muxOnKeyBinding :: MuxEnv -> MuxState -> MuxKeyCommands -> IO (Maybe MuxState)
muxOnKeyBinding env st key = do
  hPutStrLn stderr ("Key< " <> show key)
  case key of
    MuxKeySwitch -> do
      switchPuppets env st Nothing
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
    MuxKeySwitchPuppet _newIdx ->
      panic "TODO: not implemented"

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
  traverse_ syncTtySize (st ^? mst_currentPuppet . _Just . ps_process . pp_pts)
  traverse_ syncTtySize (st ^? mst_prevPuppet . _Just . ps_process . pp_pts)
  pure (Just st)
muxBody env st0 SwitchPuppet = do
  switchPuppets env st0 Nothing
muxBody env st0 (ChildExited exitedPid) = do
  case st0 ^. mst_currentPuppet of
    Nothing -> pure Nothing
    Just currSt ->
      if currSt ^. ps_process . pp_pid == exitedPid
        then do
          let newPuppets = Map.delete (_ps_idx currSt) (_mst_puppets st0)
          if _mst_keepAlive st0 || not (Map.null newPuppets)
            then
              switchPuppets
                env
                ( st0 & mst_currentPuppet .~ Nothing
                    & mst_puppets .~ newPuppets
                    & mst_syncCwdP .~ Nothing
                )
                (Just $ currSt ^. ps_mode)
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
