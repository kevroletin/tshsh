{-# LANGUAGE ViewPatterns #-}

module Tshsh.Muxer
  ( module Tshsh.Muxer.Types,
    module Tshsh.Muxer.Body,
    module Tshsh.Commands,
    module Tshsh.Puppet,
    openMuxLog,
    setupSignalHandlers,
    forkReadUserInput,
    muxLoop,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import qualified Data.Set as Set
import Data.String.Conversions
import Protolude hiding (tryIO)
import System.Posix
import System.Posix.Signals.Exts
import Tshsh.Commands
import Tshsh.Data.BufferSlice (BufferSlice (..))
import Tshsh.Muxer.Body
import Tshsh.Muxer.Log
import Tshsh.Muxer.Types
import Tshsh.Puppet
import Tshsh.ReadLoop (readLoop, readLoopStep)

muxLoop_ :: MuxEnv -> MuxState -> IO ()
muxLoop_ !env !st0 = do
  (inpMsg, readyPup) :: ([MuxCmd], Set PuppetIdx) <- waitInput

  let loop [] st = pure st
      loop (x : xs) st = handlePuppetInput x st >>= loop xs

  handleInpMsg inpMsg (Just st0)
    >>= loop (Set.toList readyPup)
    >>= \case
      Nothing -> pure ()
      Just newSt -> muxLoop_ env newSt
  where
    waitInput :: IO ([MuxCmd], Set PuppetIdx)
    waitInput =
      atomically $ do
        inpMsg <- flushTQueue (_menv_sigQueue env)
        readyPup <- swapTVar (_menv_dataAvailable env) Set.empty
        if null inpMsg && Set.null readyPup
          then retry
          else pure (inpMsg, readyPup)

    muxBodyL e s cmd = muxLog cmd >> muxBody e s cmd

    handleInpMsg [] st = pure st
    handleInpMsg _ Nothing = pure Nothing
    handleInpMsg (cmd : rest) (Just st) = muxBodyL env st cmd >>= handleInpMsg rest

    muxReadFromPuppet :: MuxState -> PuppetIdx -> IO (Maybe BufferSlice, MuxState)
    muxReadFromPuppet st idx =
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
          watchFileInput (_pp_inputH pp) (_menv_dataAvailable env) (Set.insert idx)
          pure res

    handlePuppetInput _ Nothing = pure Nothing
    handlePuppetInput idx (Just st) = do
      (mSlice, newSt) <- muxReadFromPuppet st idx
      case mSlice of
        Nothing -> pure (Just newSt)
        Just slice -> do
          muxBodyL env newSt (PuppetOutput idx slice)

muxLoop :: Mux -> IO ()
muxLoop (Mux e s) = muxLoop_ e s

setupSignalHandlers :: TQueue MuxCmd -> IO ()
setupSignalHandlers queue = do
  -- catch susp and child inpMsg
  _ <- installHandler windowChange (Catch (atomically $ writeTQueue queue WindowResize)) Nothing
  let suspendSig = atomically $ writeTQueue queue SwitchPuppet
  _ <- installHandler keyboardStop (Catch suspendSig) Nothing

  _ <- setStoppedChildFlag True
  let onChildSig :: SignalInfo -> IO ()
      onChildSig (SignalInfo _ _ NoSignalSpecificInfo) = pure ()
      onChildSig (SignalInfo _ _ SigChldInfo {..}) = do
        hPutStr stderr ("Sig> Child status: " <> show siginfoPid <> " -> " <> show siginfoStatus <> "\n" :: Text)
        atomically $ writeTQueue queue (ChildExited siginfoPid)
  _ <- installHandler processStatusChanged (CatchInfo onChildSig) Nothing
  pure ()

forkReadUserInput :: TQueue MuxCmd -> IO ()
forkReadUserInput queue = do
  void . forkIO $
    readLoop "[Read stdin thread]" stdin $ \str ->
      atomically . writeTQueue queue $ TermInput str
