module Tshsh.Muxer.SyncCwd where

import Control.Lens
import qualified Data.ByteString as BS
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

unquote_ :: Char -> Text -> Maybe Text
unquote_ c0 str0 = do
  (c1, str1) <- T.uncons str0
  (str2, c2) <- T.unsnoc str1
  if c1 == c2 && c2 == c0
    then Just (T.replace (T.pack ['\\', c0]) (T.singleton c0) str2)
    else Nothing

stripUnquote :: Text -> Text
stripUnquote (T.strip -> str) =
  fromMaybe str (unquote_ '"' str <|> unquote_ '\'' str)

runCmd :: PuppetIdx -> Text -> ProgramCont () In Out IO Text
runCmd idx cmd cont =
  Output (idx, encodeUtf8 (cmd <> "\n")) $
    let loop = WaitInput $ \(inIdx, str) ->
          if inIdx == idx
            then cont (unStrippedCmdResult str)
            else loop
     in loop

getProcessCwd :: ProcessID -> IO Text
getProcessCwd pid =
  T.strip . T.pack <$> readProcess "readlink" ["/proc/" <> show pid <> "/cwd"] []

-- TODO: too many cs conversions
syncCwdC :: PuppetState -> PuppetState -> ProgramCont_ () In Out IO
syncCwdC toSt fromSt cont0 =
  let toPid = toSt ^. ps_process . pp_pid
      fromPid = fromSt ^. ps_process . pp_pid
      toP = _ps_cfg toSt
      fromP = _ps_cfg fromSt
      toIdx = _ps_idx toSt
      fromIdx = _ps_idx fromSt
      getPrevCwd noSupportCont cont =
        case fromP ^. pc_getCwdCmd of
          GetCwdCommand cmd ->
            runCmd fromIdx cmd cont
          GetCwdFromProcess ->
            Lift (getProcessCwd fromPid) (cont . cs)
          GetCwdNoSupport ->
            noSupportCont
      tryGetCurrCwdFromProc cont =
        case toP ^. pc_getCwdCmd of
          GetCwdNoSupport -> cont Nothing
          GetCwdCommand _ -> cont Nothing
          GetCwdFromProcess ->
            Lift (getProcessCwd toPid) $ \x -> cont (Just x)
      -- TODO: copy-paste from Handlers.hs
      selectInp idx (inpIdx, x) = if idx == inpIdx then Just x else Nothing
      adapt idx p = Adapter (selectInp idx) (idx,) p
      cdToPupC cwd cont =
        case (_pc_cdCmd toP) of
          CdNoSupport -> cont
          CdSimpleCommand mkCmd -> runCmd toIdx (mkCmd cwd) (const cont)
          CdProgram act ->
            adapt (_ps_idx toSt) (act cwd (_ps_process toSt)) `AndThen` cont
   in Lift (hPutStrLn stderr ("~ SyncCwd program started" :: Text)) $ \_ ->
        getPrevCwd
          cont0
          ( \cwd' ->
              let cwd = stripUnquote cwd'
               in liftP_ (hPutStrLn stderr ("~ SyncCwd: prev cwd " <> cwd)) $
                    tryGetCurrCwdFromProc $ \mCurrCwd ->
                      let same = case mCurrCwd of
                            Nothing -> False
                            Just x -> x == cwd
                       in if same
                            then cont0
                            else cdToPupC cwd cont0
          )
