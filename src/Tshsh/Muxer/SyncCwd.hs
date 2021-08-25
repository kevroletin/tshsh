module Tshsh.Muxer.SyncCwd where

import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.String.AnsiEscapeCodes.Strip.Text
import Data.String.Conversions
import qualified Data.Text as T
import Protolude
import System.Posix (ProcessID)
import System.Process (readProcess)
import Tshsh.Commands
import Tshsh.Lang.Coroutine.CPS
import Tshsh.Puppet

type In = (PuppetIdx, StrippedCmdResult)

type Out = (PuppetIdx, BS.ByteString)

-- TODO: what about a quoted " \" "?
unquote :: ByteString -> ByteString
unquote =
  let p = (\x -> x == '"' || x == '\'')
   in C8.dropWhile p . C8.dropWhileEnd p

runCmd :: PuppetIdx -> BS.ByteString -> ProgramCont () In Out IO Text
runCmd idx cmd cont =
  Output (idx, cmd <> "\n") $
    let loop = WaitInput $ \(inIdx, str) ->
          if inIdx == idx
            then cont (unStrippedCmdResult str)
            else loop
     in loop

getProcessCwd :: ProcessID -> IO ByteString
getProcessCwd pid =
  C8.strip . C8.pack <$> readProcess "readlink" ["/proc/" <> show pid <> "/cwd"] []

-- TODO: too many cs conversions
syncCwdC :: PuppetState -> PuppetState -> ProgramCont_ () In Out IO
syncCwdC currSt prevSt cont0 =
  let currPid = currSt ^. ps_process . pp_pid
      prevPid = prevSt ^. ps_process . pp_pid
      currP = _ps_cfg currSt
      prevP = _ps_cfg prevSt
      currIdx = _ps_idx currSt
      prevIdx = _ps_idx prevSt
      getPrevCwd cont =
        case prevP ^. pc_getCwdCmd of
          GetCwdCommand cmd ->
            runCmd prevIdx (cs cmd) $ \str ->
              cont (cs . T.strip . stripAnsiEscapeCodes $ cs str)
          GetCwdFromProcess ->
            Lift (getProcessCwd prevPid) cont
      tryGetCurrCwdFromProc cont =
        case currP ^. pc_getCwdCmd of
          GetCwdCommand _ -> cont Nothing
          GetCwdFromProcess ->
            Lift (getProcessCwd currPid) $ \x -> cont (Just x)
   in Lift (hPutStrLn stderr ("~ SyncCwd program started" :: Text)) $ \_ ->
        getPrevCwd $ \cwd' ->
          let cwd = unquote cwd'
           in liftP_ (hPutStrLn stderr ("~ SyncCwd: prev cwd " <> cwd)) $
                tryGetCurrCwdFromProc $ \mCurrCwd ->
                  let same = case mCurrCwd of
                        Nothing -> False
                        Just x -> x == cwd
                   in if same
                        then cont0
                        else
                          runCmd currIdx (cs $ (currP ^. pc_mkCdCmd) (cs cwd)) $
                            const cont0
