{-# LANGUAGE StrictData #-}
{-# LANGUAGE RankNTypes #-}
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

import Lang.Coroutine.CPS

type SyncEvnState = ()

data MuxEnv = MuxEnv
  { _menv_puppets :: (Puppet, Puppet),
    _menv_logger :: Text -> IO ()
  }

data MuxState = MuxState
  { _mst_puppetSt :: (PuppetState, PuppetState),
    _mst_currentPuppetIdx :: PuppetIdx,
    _mst_currProgram :: Maybe ((), Maybe (Program () (PuppetIdx, BS.ByteString) (PuppetIdx, BS.ByteString) IO ()))
  }

data Mux = Mux { _mux_env :: MuxEnv, _mux_st :: MuxState }

$(makeLenses 'MuxState)
$(makeLenses 'MuxEnv)
$(makeLenses 'Mux)

pupIdx :: PuppetIdx -> Lens' (a, a) a
pupIdx Puppet1 = _1
pupIdx Puppet2 = _2

sortPup :: PuppetIdx -> Lens' (a, a) (a, a)
sortPup Puppet1 f (a, b) = f (a, b)
sortPup Puppet2 f (a, b) = (\(a', b') -> (b', a')) <$> f (b, a)

mst_currentPuppet :: Lens' MuxState PuppetState
mst_currentPuppet f m =
  let idx = m ^. mst_currentPuppetIdx
      ls = mst_puppetSt . pupIdx idx
  in (\x -> m & ls .~ x) <$> f (m ^. ls)

mst_backgroundPuppet :: Lens' MuxState PuppetState
mst_backgroundPuppet f m =
  let idx = nextPuppet (m ^. mst_currentPuppetIdx)
      ls = mst_puppetSt . pupIdx idx
  in (\x -> m & ls .~ x) <$> f (m ^. ls)

mst_sortedPuppets :: Lens' MuxState (PuppetState, PuppetState)
mst_sortedPuppets f m =
  let idx = m ^. mst_currentPuppetIdx
      ls = mst_puppetSt . sortPup idx
  in (\x -> m & ls .~ x) <$> f (m ^. ls)

getProcessCwd :: ProcessID -> IO Text
getProcessCwd pid =
  T.strip . T.pack <$> readProcess "readlink" ["/proc/" <> show pid <> "/cwd"] []

-- synCwdP :: PuppetIdx -> ((), Maybe (Program () (PuppetIdx, BS.ByteString) (PuppetIdx, BS.ByteString) IO ()))
-- synCwdP currPup

menv_currentPuppet :: MuxState -> Lens' MuxEnv Puppet
menv_currentPuppet st f env =
  let ls = menv_puppets . pupIdx (st ^. mst_currentPuppetIdx)
  in (\x -> env & ls .~ x) <$> f (env ^. ls)

menv_sortedPuppets :: MuxState -> Lens' MuxEnv (Puppet, Puppet)
menv_sortedPuppets st f env =
  let ls = menv_puppets . sortPup (st ^. mst_currentPuppetIdx)
  in (\x -> env & ls .~ x) <$> f (env ^. ls)

muxBody :: MuxEnv -> MuxState -> MuxCmd -> IO MuxState
muxBody env st (TermInput str) = do
  let h = env ^. menv_currentPuppet st . pup_inputH
  BS.hPut h str
  pure st
muxBody env st (PuppetOutput puppetIdx str0) =
  if puppetIdx == st ^. mst_currentPuppetIdx
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
                    (env ^. menv_logger) "Match!"
                    loop m' rest
      m' <- loop (st ^. mst_currentPuppet . ps_parser) str0

      pure (st & mst_currentPuppet . ps_parser .~ m')
    else -- TODO: what to do with background puppet output? just ignore fore now
      pure st
muxBody env st WindowResize = do
  let pup = env ^. menv_currentPuppet st
  Just (Window h w :: Window Int) <- size
  -- TODO: link c code
  _ <- system ("stty -F " <> (pup ^. pup_pts) <> " cols " <> show w <> " rows " <> show h)
  signalProcess windowChange (pup ^. pup_pid)

  pure st
muxBody env st0 SwitchPuppet = do
  let idx = nextPuppet (st0 ^. mst_currentPuppetIdx)
  let st = st0 & mst_currentPuppetIdx .~ idx
               -- & _mst_currProgram ?~ syncCwdP idx

  let (currP, prevP) = env ^. menv_sortedPuppets st

  signalProcess keyboardSignal (currP ^. pup_pid)
  currCwd <- getProcessCwd (currP ^. pup_pid)
  prevCwd <- getProcessCwd (prevP ^. pup_pid)
  when (currCwd /= prevCwd) $
    BS.hPut (currP ^. pup_inputH) (cs $ (currP ^. pup_mkCdCmd) prevCwd <> "\n")

  -- TODO: Trying to dial with bracketed paste mode
  -- ghci doesn't work with bracketed paste mode
  -- for codes see https://cirw.in/blog/bracketed-paste
  BS.hPut stdout ("\x1b[?2004l" :: BS.ByteString)

  pure st
