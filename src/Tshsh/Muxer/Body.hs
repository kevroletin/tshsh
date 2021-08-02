{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Muxer.Body where

import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Strict.Tuple
import Data.String.Conversions
import Foreign
import GHC.Base (String)
import Lang.Coroutine.CPS
import Lang.Coroutine.CPS.Folds
import Matcher.ByteString
import Matcher.Result
import Protolude hiding (hPutStrLn, log, tryIO)
import System.Console.ANSI
import System.Console.Terminal.Size
import System.Posix
import System.Posix.Signals.Exts
import System.Process
import Tshsh.Commands
import Tshsh.Muxer.Types
import Tshsh.Program.SyncCwd
import Tshsh.Puppet
import System.IO

syncTerminalSize :: String -> IO ()
syncTerminalSize pts = do
  Just (Window h w :: Window Int) <- size
  -- TODO: link c code
  _ <- system ("stty -F " <> pts <> " cols " <> show w <> " rows " <> show h)
  pure ()

muxBody :: MuxEnv -> MuxState -> MuxCmd -> IO MuxState
muxBody env st (TermInput (BufferSlice _ buff size)) = do
  let h = env ^. menv_currentPuppet st . pup_inputH
  withForeignPtr buff $ \ptr -> do
    hPutBuf h ptr size
    pure st
muxBody env st (PuppetOutput puppetIdx (BufferSlice _ buf size)) = do
  let str0 = BS.fromForeignPtr (castForeignPtr buf) 0 size

  let runProgram p = do
        let onOut (i, x) = do
              -- TODO: should we push output into muxer queue?
              let h = env ^. menv_puppets . pupIdx i . pup_inputH
              BS.hPut h x
        feedInputM onOut (puppetIdx, BS.copy str0) p >>= \case
          Cont p' -> pure (Just p')
          Res r -> do
            (env ^. menv_logger) $ "Program terminated with: " <> show r
            pure Nothing

  let feedMatcher !foundPosEnd !m !str =
        case matchStr m str of
          NoMatch m' -> (foundPosEnd, m')
          Match m' len _ rest ->
            if BS.null rest
              then (0, m')
              else do
                feedMatcher (BS.length rest) m' rest

  let runMatchers currP = do
        let (promptPos, m') = feedMatcher (-1) (currP ^. ps_parser) str0
        let (clrScrPos, mc') = feedMatcher (-1) (currP ^. ps_clrScrParser) str0

        when (promptPos >= 0) $
          (env ^. menv_logger) ("=== Prompt detected. offset: " <> show promptPos <> "; " <> show puppetIdx)
        when (clrScrPos >= 0) $
          (env ^. menv_logger) ("=== ClrScr detected. offset: " <> show clrScrPos <> "; " <> show puppetIdx)

        let mode = case (promptPos, clrScrPos) of
              (-1, -1) -> currP ^. ps_mode
              (_, -1) -> PuppetModeRepl
              (-1, _) -> PuppetModeTUI
              (p, c) -> if p > c then PuppetModeTUI else PuppetModeRepl
        when (mode /= currP ^. ps_mode) $
          (env ^. menv_logger) ("=== Mode has changed: " <> show mode <> "; " <> show puppetIdx)

        pure
          ( currP
              { _ps_parser = m',
                _ps_clrScrParser = mc',
                _ps_mode = mode
              }
          )

  when (puppetIdx == st ^. mst_currentPuppetIdx) $
    BS.hPut stdout str0

  nextCp <- join <$> traverse runProgram (st ^. mst_currentProgram)
  nextPup <- runMatchers (st ^. mst_currentPuppet)

  pure
    ( st & mst_currentProgram .~ nextCp
        & mst_currentPuppet .~ nextPup
    )
muxBody env st WindowResize = do
  let pup = env ^. menv_currentPuppet st
  syncTerminalSize (pup ^. pup_pts)
  pure st
muxBody env st0 SwitchPuppet = do
  let idx = nextPuppet (st0 ^. mst_currentPuppetIdx)
  let st = st0 & mst_currentPuppetIdx .~ idx
  let currP = env ^. menv_currentPuppet st

  -- TODO: a hack. Finxing paste from X clipboard in ghci
  -- Paste stops working in GHCI if bracket mode is enabled. Zsh enables bracket paste
  -- mode each time it prints a prompt (at least in our setup with zprezto).
  -- see https://cirw.in/blog/bracketed-paste
  BS.hPut stdout ("\x1b[?2004l" :: BS.ByteString)

  let program = () :!: syncCwdP env idx (Finish (Right ()))

  let (from :!: to) = st0 ^. mst_sortedPuppets
  mProgram <- case (_ps_mode from, _ps_mode to) of
    (_, PuppetModeTUI) -> do
      -- returning into tui -> send C-l to with the hope that tui app will redraw itself
      BS.hPut (currP ^. pup_inputH) ("\f" :: BS.ByteString)
      pure Nothing
    (PuppetModeRepl, PuppetModeRepl) -> do
      -- switching between repls -> send C-c with the hope that repl will render a prompt
      signalProcess keyboardSignal (currP ^. pup_pid)
      pure (Just program)
    (PuppetModeTUI, PuppetModeRepl) -> do
      -- clear tui interface, try to redraw repl prompt by sending C-c
      BS.hPut stdout "\ESC[H\ESC[2J" -- move cursor to (0,0) clearScreen
      showCursor
      signalProcess keyboardSignal (currP ^. pup_pid)
      pure (Just program)

  pure (st & mst_currentProgram .~ mProgram)
