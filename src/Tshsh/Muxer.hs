{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Muxer where

import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import Data.String.Conversions
import qualified Data.Text as T
import Foreign
import Matcher.ByteString
import Protolude hiding (hPutStrLn, log, tryIO)
import System.Console.Terminal.Size
import System.Posix
import System.Posix.Signals.Exts
import System.Process
import Tshsh.Commands
import Tshsh.Puppet

data MuxState = MuxState
  { _mux_puppets :: (Puppet, Puppet),
    _mux_currentPuppetIdx :: PuppetIdx,
    _mux_logger :: Text -> IO ()
  }

$(makeLenses 'MuxState)

currentPuppet :: Lens' MuxState Puppet
currentPuppet f (MuxState (a, b) idx lg) =
  case idx of
    Puppet1 -> (\a' -> MuxState (a', b) idx lg) <$> f a
    Puppet2 -> (\b' -> MuxState (a, b') idx lg) <$> f b

backgroundPuppet :: Lens' MuxState Puppet
backgroundPuppet f (MuxState (a, b) idx lg) =
  case idx of
    Puppet2 -> (\a' -> MuxState (a', b) idx lg) <$> f a
    Puppet1 -> (\b' -> MuxState (a, b') idx lg) <$> f b

sortedPuppets :: Lens' MuxState (Puppet, Puppet)
sortedPuppets f (MuxState (a, b) idx lg) =
  case idx of
    Puppet1 -> (\(a', b') -> MuxState (a', b') idx lg) <$> f (a, b)
    Puppet2 -> (\(a', b') -> MuxState (b', a') idx lg) <$> f (b, a)

getProcessCwd :: ProcessID -> IO Text
getProcessCwd pid =
  T.strip . T.pack <$> readProcess "readlink" ["/proc/" <> show pid <> "/cwd"] []

muxBody :: MuxState -> MuxCmd -> IO MuxState
muxBody st (TermInput str) = do
  let Puppet {..} = st ^. currentPuppet
  BS.hPut _pup_inputH str
  pure st
muxBody st@MuxState {..} (PuppetOutput puppetIdx str0) =
  if puppetIdx == _mux_currentPuppetIdx
    then do
      BS.hPut stdout str0

      -- TODO: do we want to do parsing in a main loop? maybe do it asynchronously?
      let loop m str =
            case matchStr m str of
              NoMatch m' -> pure m'
              Match m' _ rest ->
                if BS.null rest
                  then pure m'
                  else do
                    _mux_logger "Match!"
                    loop m' rest
      m' <- loop (st ^. currentPuppet . pup_parser) str0

      pure (st & currentPuppet . pup_parser .~ m')
    else -- TODO: what to do with background puppet output? just ignore fore now
      pure st
muxBody st WindowResize = do
  let Puppet {..} = st ^. currentPuppet
  Just (Window h w :: Window Int) <- size
  -- TODO: link c code
  _ <- system ("stty -F " <> _pup_pts <> " cols " <> show w <> " rows " <> show h)
  signalProcess windowChange _pup_pid

  pure st
muxBody st0 SwitchPuppet = do
  let st = st0 & mux_currentPuppetIdx %~ nextPuppet

  -- let (currP, prevP) = getPuppetsInOrder st
  let (currP, prevP) = st ^. sortedPuppets

  signalProcess keyboardSignal (_pup_pid currP)
  currCwd <- getProcessCwd (_pup_pid currP)
  prevCwd <- getProcessCwd (_pup_pid prevP)
  when (currCwd /= prevCwd) $
    BS.hPut (_pup_inputH currP) (cs $ _pup_mkCdCmd currP prevCwd <> "\n")

  -- print _mux_currentPuppetIdx
  -- TODO: we should parse Unicode, otherwise we can break it on switch
  -- TODO: redraw last prompt

  -- TODO: Trying to dial with bracketed paste mode
  -- ghci doesn't work with bracketed paste mode
  -- for codes see https://cirw.in/blog/bracketed-paste
  BS.hPut stdout ("\x1b[?2004l" :: BS.ByteString)
  -- probably need to dump everything a pup dumps after initialization
  -- cause it set's up a terminal.
  -- We set raw mode on master and don't care about line discipline
  -- We deal with signals in the other place
  -- We just ignored flow control for now
  -- Now the problem that a puppet sets some parameters on it's virtual tty and
  -- we should set it to the main termina

  pure st
