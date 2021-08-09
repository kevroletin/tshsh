{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Tshsh.Program.SyncCwd where

import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Strict.Tuple
import Data.String.AnsiEscapeCodes.Strip.Text
import Data.String.Conversions
import qualified Data.Text as T
import Lang.Coroutine.CPS
import Matcher.ByteString
import Matcher.Result
import Protolude
import System.Posix (ProcessID)
import System.Process (readProcess)
import Tshsh.Commands
import Tshsh.Muxer.Types
import Tshsh.Puppet

type In = (PuppetIdx, CmdResultOutput)

type Out = (PuppetIdx, BS.ByteString)

unquote :: ByteString -> ByteString
unquote =
  let p = (\x -> x == '"' || x == '\'')
   in C8.dropWhile p . C8.dropWhileEnd p

runCmd :: MuxEnv -> PuppetIdx -> BS.ByteString -> ProgramCont () In Out IO Text
runCmd env idx cmd cont =
  Output (idx, cmd <> "\n") $
    let loop = WaitInput $ \(inIdx, str) ->
                 if inIdx == idx
                   then cont (unCmdResultOutput str)
                   else loop
    in loop

getProcessCwd :: ProcessID -> IO ByteString
getProcessCwd pid =
  C8.strip . C8.pack <$> readProcess "readlink" ["/proc/" <> show pid <> "/cwd"] []

-- TODO: too many cs conversions
syncCwdC :: Pair ProcessID ProcessID -> MuxEnv -> PuppetIdx -> ProgramCont' () In Out IO
syncCwdC (currPid :!: prevPid) env idx cont0 =
  let prevIdx = nextPuppet idx
      (currP :!: prevP) = env ^. menv_puppets . sortPup idx
      getCwd cont =
        case prevP ^. pup_getCwdCmd of
          GetCwdCommand cmd ->
            runCmd env prevIdx (cs cmd) $ \str ->
              cont (cs . T.strip . stripAnsiEscapeCodes $ cs str)
          GetCwdFromProcess ->
            Lift (getProcessCwd prevPid) cont
   in
    Lift (hPutStrLn stderr ("~ Sync env program started" :: Text)) $ \_ ->
      getCwd $ \cwd' ->
          let cwd = unquote cwd'
          in Lift (hPutStrLn stderr ("~ SyncCwd: prev cwd " <> cwd)) $ \() ->
                runCmd env idx (cs $ (currP ^. pup_mkCdCmd) (cs cwd)) $
                  const cont0
