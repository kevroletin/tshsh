{-# LANGUAGE ViewPatterns #-}

module Tshsh.Muxer
  ( module Tshsh.Muxer.Types,
    module Tshsh.Commands,
    module Tshsh.Puppet,
    openMuxLog,
    tshshMain,
  )
where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.String.Conversions
import Protolude hiding (tryIO)
import System.Posix
import System.Posix.Signals.Exts
import Tshsh.Commands
import Tshsh.Data.BufferSlice (BufferSlice (..))
import qualified Tshsh.Data.BufferSlice as BufferSlice
import Tshsh.KeyParser
import qualified Tshsh.Muxer.Handlers as Handlers
import Tshsh.Muxer.Log
import Tshsh.Muxer.PuppetProcess
import Tshsh.Muxer.Types
import Tshsh.Puppet
import Tshsh.ReadLoop
import Tshsh.Tty

muxLoop :: MuxEnv -> MuxState -> IO ()
muxLoop !env !st0 = do
  (sigMsg, termInpAv, readyPup) <- waitInput

  let handlAllOut [] st = pure st
      handlAllOut (x : xs) st = do
        handlePuppetOutput x st >>= handlAllOut xs

  handleSigMsg sigMsg (Just st0)
    >>= handlAllOut (Set.toList readyPup)
    >>= whenAv termInpAv handleTermInput
    >>= \case
      Nothing -> pure ()
      Just newSt -> muxLoop env newSt
  where
    whenAv False _ st = pure st
    whenAv True act st = act st

    waitInput :: IO ([MuxSignal], Bool, Set PuppetIdx)
    waitInput =
      atomically $ do
        sigMsg <- flushTQueue (_menv_sigQueue env)
        termInp <- swapTVar (_menv_inputAvailable env) False
        readyPup <- swapTVar (_menv_outputAvailable env) Set.empty
        -- pure (sigMsg, termInp, readyPup)
        if null sigMsg && not termInp && Set.null readyPup
          then retry
          else pure (sigMsg, termInp, readyPup)

    handleSigMsg [] st = pure st
    handleSigMsg _ Nothing = pure Nothing
    handleSigMsg (cmd : rest) (Just st) = Handlers.onSignal env st cmd >>= handleSigMsg rest

    readTermInput :: MuxState -> IO (Maybe BufferSlice, MuxState)
    readTermInput st = do
      res <-
        readLoopStep (_mst_readInputSt st) >>= \case
          Nothing -> do
            pure (Nothing, st)
          Just (slice, newReadSt) -> do
            pure (Just slice, st {_mst_readInputSt = newReadSt})
      watchFileInput stdin (_menv_inputAvailable env) (const True)
      pure res

    handleTermInput :: Maybe MuxState -> IO (Maybe MuxState)
    handleTermInput Nothing = pure Nothing
    handleTermInput (Just st1) = do
      (mSlice, newSt) <- readTermInput st1
      case mSlice of
        Nothing -> pure (Just newSt)
        Just (BufferSlice.sliceToByteString -> str0) -> do
          let loop _ Nothing = pure Nothing
              loop res (Just st) =
                case res of
                  KeyParserData out next ->
                    loop next =<< Handlers.onTermInput st out
                  KeyParserAction act next -> do
                    loop next =<< Handlers.onKeyBinding env st act
                  KeyParserNull next ->
                    pure $ Just (next, st)
          loop (keyParserRun (_mst_inputParser st1) str0) (Just st1) >>= \case
            Nothing ->
              pure $ Just newSt
            Just (newKeyParser, newSt2) ->
              pure $ Just (newSt2 {_mst_inputParser = newKeyParser})

    readPuppetOutput :: MuxState -> PuppetIdx -> IO (Maybe BufferSlice, MuxState)
    readPuppetOutput st idx = do
      case st ^? mst_puppets . ix idx of
        Nothing -> pure (Nothing, st)
        Just (_ps_process -> pp) -> do
          res <-
            readLoopStep (_pp_readSliceSt pp) >>= \case
              Nothing -> pure (Nothing, st)
              Just (slice, newReadSt) -> do
                let newSt =
                      st & mst_puppets . ix idx . ps_process
                        .~ (pp {_pp_readSliceSt = newReadSt})
                pure (Just slice, newSt)
          watchFileInput (_pp_inputH pp) (_menv_outputAvailable env) (Set.insert idx)
          pure res

    handlePuppetOutput _ Nothing = pure Nothing
    handlePuppetOutput idx (Just st) = do
      (mSlice, newSt) <- readPuppetOutput st idx
      case mSlice of
        Nothing -> pure (Just newSt)
        Just slice -> do
          res <- Handlers.onPuppetOutput env newSt idx slice
          pure res

tshshMain :: TshshCfg -> IO ()
tshshMain TshshCfg {..} = do
  kb <- case mkKeyParser _tsh_keyBindings of
    Left err -> do
      putStrLn ("Malformed keybinding cfg:  " <> show err :: Text)
      exitFailure
    Right p -> pure p

  let idx1 = _tsh_firstPuppetIdx
      idx2 = _tsh_secondPuppetIdx
      Just cfg1 = Map.lookup idx1 _tsh_puppets

  bracket
    configureStdinTty
    restoreStdinTty
    $ \_ -> do
      inAvailable <- newTVarIO False
      readInputSt <- readLoopInit stdin
      watchFileInput stdin inAvailable (const True)
      outAvailable <- newTVarIO Set.empty
      sigQueue <- newTQueueIO

      setupSignalHandlers sigQueue

      pup1st <- startPuppetProcess Nothing outAvailable idx1 cfg1

      let env =
            MuxEnv
              { _menv_puppets = _tsh_puppets,
                _menv_defaultPuppet = idx1,
                _menv_outputAvailable = outAvailable,
                _menv_inputAvailable = inAvailable,
                _menv_sigQueue = sigQueue
              }
      let st =
            MuxState
              { _mst_puppets = Map.fromList [(idx1, pup1st)],
                _mst_currentPuppetIdx = idx1,
                _mst_prevPuppetIdx = idx2,
                _mst_syncCwdP = Nothing,
                _mst_keepAlive = False,
                _mst_inputParser = kb,
                _mst_prevCmdOut = StrippedCmdResult "",
                _mst_readInputSt = readInputSt
              }

      muxLoop env st

setupSignalHandlers :: TQueue MuxSignal -> IO ()
setupSignalHandlers queue = do
  _ <- installHandler windowChange (Catch (atomically $ writeTQueue queue WindowResize)) Nothing

  _ <- setStoppedChildFlag True
  let onChildSig :: SignalInfo -> IO ()
      onChildSig (SignalInfo _ _ NoSignalSpecificInfo) = pure ()
      onChildSig (SignalInfo _ _ SigChldInfo {..}) = do
        hPutStr stderr ("Sig> Child status: " <> show siginfoPid <> " -> " <> show siginfoStatus <> "\n" :: Text)
        atomically $ writeTQueue queue (ChildExited siginfoPid)
  _ <- installHandler processStatusChanged (CatchInfo onChildSig) Nothing
  pure ()
