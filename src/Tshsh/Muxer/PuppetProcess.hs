module Tshsh.Muxer.PuppetProcess
  ( startPuppetProcess,
  )
where

import Control.Concurrent.STM
import qualified Data.Set as Set
import Data.Strict.Tuple.Extended
import Data.String.Conversions
import qualified Data.Text as T
import Protolude
import System.Posix
import System.Process
import Tshsh.Commands
import qualified Tshsh.Data.BufferSlice as BufferSlice
import Tshsh.Lang.Coroutine.CPS
import Tshsh.Muxer.ShellOutputParser
import qualified Tshsh.Muxer.TuiModeMatcher as TuiMatcher
import Tshsh.Puppet
import Tshsh.ReadLoop
import Tshsh.Tty

startPuppetProcess ::
  Maybe (Text) ->
  Maybe ([(Text, Text)]) ->
  TVar (Set PuppetIdx) ->
  PuppetIdx ->
  PuppetCfg ->
  IO PuppetState
startPuppetProcess mCwd mEnv dataAvail idx cfg@PuppetCfg {..} = do
  let outParser = toEv (raceMatchersP `pipe` accumCmdOutP `pipe` stripCmdOutP)

  let outParserSt =
        OutputParserSt
          { _op_promptMatcher = _pc_promptMatcher,
            _op_tuiModeMatcher = TuiMatcher.tuiModeMatcher,
            _op_mode = _pc_initMode,
            _op_currCmdOut = RawCmdResult BufferSlice.listEmpty
          }

  (master, slave) <- openPseudoTerminal
  masterH <- fdToHandle master
  slaveH <- fdToHandle slave
  pts <- getSlaveTerminalName master

  -- _ <- system ("stty -F " <> pts <> " $(stty --save)")
  syncTtySize pts

  (_, _, _, p) <-
    createProcess
      -- To implement jobs control a process needs
      -- 1. to become a session leader
      -- 2. to acquire a controlling terminal so that it's children inherit the same terminal
      (proc "acquire_tty_wrapper" (fmap cs (_pc_cmd : _pc_cmdArgs)))
        { cwd = T.unpack <$> mCwd,
          env = (\env -> [(T.unpack k, T.unpack v) | (k, v) <- env]) <$> mEnv,
          std_in = UseHandle slaveH,
          std_out = UseHandle slaveH,
          std_err = UseHandle slaveH
          -- new_session = True,
          -- create_group = False,
          -- this one is not implemented in rts that why we wrote an acquire_tty_wrapper
          -- aquire_tty = True
        }
  -- TODO: handle process startup failures
  (Just pid) <- getPid p

  readSlice <- readLoopInit masterH

  watchFileInput masterH dataAvail (Set.insert idx)

  hPutStrLn stderr ("Started: " <> (show pid :: Text))
  pure $
    PuppetState
      { _ps_idx = idx,
        _ps_cfg = cfg,
        _ps_outputParser = (outParserSt :!: outParser),
        _ps_process =
          PuppetProcess
            { _pp_handle = p,
              _pp_pid = pid,
              _pp_inputH = masterH,
              _pp_pts = pts,
              _pp_readSliceSt = readSlice
            }
      }
