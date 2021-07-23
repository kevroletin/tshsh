{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Muxer.Body where

import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import Data.String.Conversions
import Foreign
import Lang.Coroutine.CPS
import Lang.Coroutine.CPS.Folds
import Matcher.ByteString
import Protolude hiding (hPutStrLn, log, tryIO)
import System.Console.Terminal.Size
import System.Posix
import System.Posix.Signals.Exts
import System.Process
import Tshsh.Commands
import Tshsh.Muxer.Types
import Tshsh.Program.SyncCwd
import Tshsh.Puppet

muxBody :: MuxEnv -> MuxState -> MuxCmd -> IO MuxState
muxBody env st (TermInput str) = do
  let h = env ^. menv_currentPuppet st . pup_inputH
  BS.hPut h str
  pure st
muxBody env st (PuppetOutput puppetIdx str0) = do
  nextCp <-
    case st ^. mst_currentProgram of
      Nothing -> pure Nothing
      Just p -> do
        let onOut (i, x) = do
              -- TODO: should we push output into muxer queue?
              let h = env ^. menv_puppets . pupIdx i . pup_inputH
              BS.hPut h x
        feedInputM onOut (puppetIdx, str0) p >>= \case
          Cont p' -> pure (Just p')
          Res r -> do
            (env ^. menv_logger) $ "Program terminated with: " <> show r
            pure Nothing

  res <-
    if puppetIdx == st ^. mst_currentPuppetIdx
      then do
        BS.hPut stdout str0

        let loop m str =
              case matchStr m str of
                NoMatch m' -> pure m'
                Match m' len _ rest -> do
                  (env ^. menv_logger) ("=== Prompt detected. len: " <> show len <> "; " <> show puppetIdx)
                  if BS.null rest
                    then pure m'
                    else do
                      loop m' rest
        m' <- loop (st ^. mst_currentPuppet . ps_parser) str0
        pure (st & mst_currentPuppet . ps_parser .~ m')
      else -- TODO: what to do with background puppet output? just ignore for now
        pure st
  pure (res & mst_currentProgram .~ nextCp)
muxBody env st WindowResize = do
  let pup = env ^. menv_currentPuppet st
  Just (Window h w :: Window Int) <- size
  -- TODO: link c code
  _ <- system ("stty -F " <> (pup ^. pup_pts) <> " cols " <> show w <> " rows " <> show h)
  signalProcess windowChange (pup ^. pup_pid)

  pure st
muxBody env st0 SwitchPuppet = do
  let idx = nextPuppet (st0 ^. mst_currentPuppetIdx)
  let st =
        st0 & mst_currentPuppetIdx .~ idx
          & mst_currentProgram ?~ ((), syncCwdP env idx (Finish (Right ())))

  let currP = env ^. menv_currentPuppet st

  -- TODO: a hack. Finxing paste from X clipboard in ghci
  -- Paste stops working in GHCI if bracket mode is enabled. Zsh enables bracket paste
  -- mode each time it prints a prompt (at least in our setup with zprezto).
  -- see https://cirw.in/blog/bracketed-paste
  BS.hPut stdout ("\x1b[?2004l" :: BS.ByteString)

  -- TODO: Here we force a shell to redraw it's prompt by sending SIGINT
  -- (usually triggered by ^C). It will interrupt running commands, though,
  -- so it will cancel running commands.
  signalProcess keyboardSignal (currP ^. pup_pid)

  pure st
