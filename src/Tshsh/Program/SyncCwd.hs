{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Tshsh.Program.SyncCwd where

import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.String.AnsiEscapeCodes.Strip.Text
import Data.String.Conversions
import qualified Data.Text as T
import Lang.Coroutine.CPS
import Matcher.ByteString
import Protolude
import System.Posix (ProcessID)
import System.Process (readProcess)
import Tshsh.Commands
import Tshsh.Muxer.Types
import Tshsh.Puppet
import Data.Strict.Tuple

type In = (PuppetIdx, BS.ByteString)

type Out = (PuppetIdx, BS.ByteString)

dropEnd :: Int -> ByteString -> ByteString
dropEnd n str = BS.take (BS.length str - n) str

takeEnd :: Int -> ByteString -> ByteString
takeEnd n str = BS.drop (BS.length str - n) str

readUntilPrompt :: MuxEnv -> PuppetIdx -> ProgramCont () In Out IO BS.ByteString
readUntilPrompt env idx cont =
  let pup = env ^. menv_puppets . pupIdx idx
      loop !res !matcher = WaitInput $ \(s, i) ->
        if s /= idx
          then loop res matcher
          else case matchStr matcher i of
            Match _m len prev _rest ->
              cont (dropEnd len . mconcat . reverse $ prev : res)
            NoMatch m -> loop (i : res) m
   in loop [] (pup ^. pup_promptParser)

removeFirstLine :: BS.ByteString -> BS.ByteString
removeFirstLine str =
  let (a, b) = BS.breakSubstring "\n" str
   in if BS.null a
        then b
        else BS.drop 1 b

unquote :: ByteString -> ByteString
unquote =
  let p = (\x -> x == '"' || x == '\'')
   in C8.dropWhile p . C8.dropWhileEnd p

runCmd :: MuxEnv -> PuppetIdx -> BS.ByteString -> ProgramCont () In Out IO BS.ByteString
runCmd env idx cmd cont =
  Output (idx, cmd <> "\n") $
    readUntilPrompt env idx $ \str ->
      cont (C8.dropWhileEnd isSpace . removeFirstLine $ str)

getProcessCwd :: ProcessID -> IO ByteString
getProcessCwd pid =
  C8.strip . C8.pack <$> readProcess "readlink" ["/proc/" <> show pid <> "/cwd"] []

-- TODO: too many cs conversions
syncCwdP :: MuxEnv -> PuppetIdx -> ProgramCont' () In Out IO
syncCwdP env idx cont0 =
  let prevIdx = nextPuppet idx
      (currP :!: prevP) = env ^. menv_puppets . sortPup idx
      getCwd cont =
        case prevP ^. pup_getCwdCmd of
          GetCwdCommand cmd ->
            runCmd env prevIdx (cs cmd) $ \str ->
              cont (cs . T.strip . stripAnsiEscapeCodes $ cs str)
          GetCwdFromProcess ->
            Lift (getProcessCwd (prevP ^. pup_pid)) cont
   in getCwd $ \cwd' ->
        let cwd = unquote cwd'
         in Lift ((env ^. menv_logger) ("prev cwd(" <> cs cwd <> ")")) $ \() ->
              runCmd env idx (cs $ (currP ^. pup_mkCdCmd) (cs cwd)) $
                const cont0
