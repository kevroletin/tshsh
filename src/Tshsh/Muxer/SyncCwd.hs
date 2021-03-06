{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Muxer.SyncCwd where

import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Protolude
import System.Posix (ProcessID)
import System.Process (readProcess)
import Tshsh.Commands
import Tshsh.Lang.Coroutine.CPS
import Tshsh.Puppet

type In = (PuppetIdx, StrippedCmdResult)

type Out = (PuppetIdx, BS.ByteString)

adaptPuppetAct :: PuppetState -> Program () i t m r -> Program st (PuppetIdx, i) (PuppetIdx, t) m r
adaptPuppetAct pupSt = adapt (_ps_idx pupSt)
  where
    selectInp idx (inpIdx, x) = if idx == inpIdx then Just x else Nothing
    adapt idx p = AdapterAll united (selectInp idx) (idx,) p

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

runCmd :: PuppetIdx -> Text -> ProgramCont st In Out IO ByteString r
runCmd idx cmd cont =
  Output (idx, encodeUtf8 (cmd <> "\n")) $
    let loop = $waitInputDefC $ \(inIdx, str) ->
          if inIdx == idx
            then cont (unStrippedCmdResult str)
            else loop
     in loop

getProcessCwd :: ProcessID -> IO Text
getProcessCwd pid =
  T.strip . T.pack <$> readProcess "readlink" ["/proc/" <> show pid <> "/cwd"] []

getPuppetCwd :: PuppetState -> ProgramCont st In Out IO (Maybe Text) r
getPuppetCwd st cont =
  case st ^. ps_cfg . pc_getCwdCmd of
    GetCwdCommand cmd ->
      runCmd (st ^. ps_idx) cmd (cont . Just . stripUnquote . decodeUtf8)
    GetCwdFromProcess ->
      Lift (getProcessCwd (st ^. ps_process . pp_pid)) (cont . Just)
    GetCwdNoSupport ->
      (cont Nothing)

tryGetCurrCwdFromProc :: PuppetState -> ProgramCont st i o IO (Maybe Text) r
tryGetCurrCwdFromProc pupSt cont =
  case pupSt ^. ps_cfg . pc_getCwdCmd of
    GetCwdNoSupport -> cont Nothing
    GetCwdCommand _ -> cont Nothing
    GetCwdFromProcess ->
      Lift (getProcessCwd (pupSt ^. ps_process . pp_pid)) (cont . Just)

puppetCdC :: PuppetState -> Maybe Text -> ProgramCont_ () In Out IO r
puppetCdC pupSt mCwd cont0 =
  case mCwd of
    Nothing -> cont0
    Just cwd ->
      tryGetCurrCwdFromProc pupSt $ \mCurrCwd ->
        let same = Just True == ((cwd ==) <$> mCurrCwd)
         in if same
              then cont0
              else cdC cwd cont0
  where
    cdC cwd cont =
      case (pupSt ^. ps_cfg . pc_cdCmd) of
        CdNoSupport -> cont
        CdSimpleCommand mkCmd -> runCmd (pupSt ^. ps_idx) (mkCmd cwd) (const cont)
        CdProgram act ->
          adaptPuppetAct pupSt (act cwd (_ps_process pupSt)) `andThenP_` cont

syncCwdC :: PuppetState -> PuppetState -> ProgramCont_ () In Out IO r
syncCwdC toSt fromSt cont0 =
  liftP_ (hPutStrLn stderr ("~ SyncCwd program started" :: Text)) $
    getPuppetCwd fromSt $ \mCwd ->
      puppetCdC toSt mCwd $
        cont0
