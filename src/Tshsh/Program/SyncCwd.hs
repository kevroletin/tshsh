{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Tshsh.Program.SyncCwd where

import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.String.Conversions
import Lang.Coroutine.CPS
import Matcher.ByteString
import Protolude
import Tshsh.Commands
import Tshsh.Muxer.Types
import Tshsh.Puppet

type In = (PuppetIdx, BS.ByteString)

type Out = (PuppetIdx, BS.ByteString)

readUntilPrompt :: MuxEnv -> PuppetIdx -> ProgramCont () In Out IO BS.ByteString
readUntilPrompt env idx cont =
  let pup = env ^. menv_puppets . pupIdx idx
      loop !res !matcher = WaitInput $ \(s, i) ->
        if s /= idx
          then loop res matcher
          else case matchStr matcher i of
            Match _m prev _rest ->
              cont (mconcat . reverse $ prev : res)
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

syncCwdP :: MuxEnv -> PuppetIdx -> ProgramCont' () In Out IO
syncCwdP env idx cont =
  let (currP, prevP) = env ^. menv_puppets . sortPup idx
   in runCmd env (nextPuppet idx) (cs $ prevP ^. pup_getCwdCmd) $ \cwd' ->
        let cwd = unquote cwd'
         in runCmd env idx (cs $ (currP ^. pup_mkCdCmd) (cs cwd)) $
            const cont
