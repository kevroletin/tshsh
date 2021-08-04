{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Tshsh.Muxer.Body where

import Control.Lens
import Control.Monad
import Data.BufferSlice (BufferSlice (..), SliceList (..))
import qualified Data.BufferSlice as BufferSlice
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Strict.Tuple
import Data.String.AnsiEscapeCodes.Strip.Text
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Foreign
import GHC.Base (String)
import Lang.Coroutine.CPS
import Lang.Coroutine.CPS.Folds
import Matcher.ByteString
import Matcher.Result
import Protolude hiding (hPutStrLn, log, tryIO)
import System.Console.ANSI
import System.Console.Terminal.Size
import System.IO
import System.Posix
import System.Posix.Signals.Exts
import System.Process
import Tshsh.Commands
import Tshsh.Muxer.Types
import Tshsh.Program.SyncCwd
import Tshsh.Puppet

syncTerminalSize :: String -> IO ()
syncTerminalSize pts = do
  Just (Window h w :: Window Int) <- size
  -- TODO: link c code
  _ <- system ("stty -F " <> pts <> " cols " <> show w <> " rows " <> show h)
  pure ()

copyToXClipboard :: Text -> IO ()
copyToXClipboard str = do
  (Just inP, _, _, _) <- createProcess $ (proc "xclip" ["-selection", "clipboard", "-in"]) {std_in = CreatePipe}
  T.hPutStr inP str
  hClose inP

stripCmdOut :: BufferSlice.SliceList -> Text
stripCmdOut list =
  let txt = cs $ BufferSlice.listConcat list
   in T.strip . T.drop 1 . Protolude.snd . T.break (== '\n') . stripAnsiEscapeCodes $ txt

muxBody :: MuxEnv -> MuxState -> MuxCmd -> IO MuxState
muxBody env st (TermInput (BufferSlice _ buff size)) = do
  let h = env ^. menv_currentPuppet st . pup_inputH
  withForeignPtr buff $ \ptr -> do
    hPutBuf h ptr size
    pure st
muxBody env st (PuppetOutput puppetIdx inp@(BufferSlice inpSliceId buf size)) = do
  (env ^. menv_logger) "--- Puppet output:\n"
  (env ^. menv_logger) (show . stripAnsiEscapeCodes . cs . BufferSlice.sliceToByteString $ inp)
  (env ^. menv_logger) "\n---\n"

  let str0 = BS.fromForeignPtr buf 0 size

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

  let feedMatcher :: Monad m => (st -> Int -> ByteString -> ByteString -> m st) -> (st -> m st) -> st -> SomeMatcher -> ByteString -> m (st, SomeMatcher)
      feedMatcher onMatch onNoMatch !st !m !str =
        case matchStr m str of
          NoMatch m' -> (,m') <$> onNoMatch st
          Match m' len prev rest ->
            if BS.null rest
              then (,m') <$> onMatch st len prev rest
              else do
                newSt <- onMatch st len prev rest
                feedMatcher onMatch onNoMatch newSt m' rest

  let runMatchers currP = do
        -- TODO: don't accumulate output when we are in tui mode
        let ((promptPos, prevCmdOut, currCmdOut), m') =
              runIdentity $
                feedMatcher
                  ( \(_, prev, curr) len str strRest -> do
                      let slice = BufferSlice.sliceFromByteString inpSliceId str
                      pure
                        ( BS.length strRest,
                          BufferSlice.listDropEnd len $ BufferSlice.listAppendEnd curr slice,
                          BufferSlice.listEmpty
                        )
                  )
                  (\(pos, prev, curr) -> pure (pos, prev, BufferSlice.listAppendEnd curr inp))
                  (-1, currP ^. ps_prevCmdOut, currP ^. ps_currCmdOut)
                  (currP ^. ps_parser)
                  str0
        let (clrScrPos, mc') =
              runIdentity $
                feedMatcher (\_ _ _ rest -> pure (BS.length rest)) pure (-1) (currP ^. ps_clrScrParser) str0

        -- log and copy to clipboard
        when (promptPos >= 0) $ do
          (env ^. menv_logger) ("=== Prompt detected. offset: " <> show promptPos <> "; " <> show puppetIdx <> "\n")
          (env ^. menv_logger) "=== stripped output of the last cmd:\n"

          let stripped = stripCmdOut prevCmdOut
          (env ^. menv_logger) stripped
          (env ^. menv_logger) "\n"

          unless (T.null stripped) (copyToXClipboard stripped)

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
                _ps_mode = mode,
                _ps_currCmdOut = currCmdOut,
                _ps_prevCmdOut = prevCmdOut
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

  let copyPrevCmdC = Lift . copyToXClipboard . stripCmdOut $ st0 ^. mst_currentPuppet . ps_prevCmdOut
      copyPrevCmd = copyPrevCmdC $ \_ -> Finish (Right ())
      program = () :!: syncCwdP env idx copyPrevCmd

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
