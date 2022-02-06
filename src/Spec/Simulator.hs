{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Simulator where

import Control.Lens
import Control.Monad
import qualified Data.Map as M
import Data.Strict.Tuple
import qualified Data.Text as T
import Protolude hiding (exp, log, (>>), (>>=))
import Spec.CPS.Folds (evalProgramM)
import Tshsh.Lang.Coroutine.CPS
import qualified Prelude as P
import Data.Time

data Shell = Shell_1 | Shell_2
  deriving (Eq, Ord, Show)

data Input
  = Prompt
  | TextInput Text
  deriving (Show)

parseEnv :: Text -> Either Text [(Text, Text)]
parseEnv str = traverse parseLine (T.lines str)
  where
    parseLine :: Text -> Either Text (Text, Text)
    parseLine x =
      let (a, b) = T.breakOn "=" x
       in if T.null a
            then Left ("parseEvn: missing = " <> str)
            else Right (a, T.drop 1 b)

finishOnErr :: Either Text s -> ProgramCont st i o m s r
finishOnErr (Left err) _ = Finish (Left err)
finishOnErr (Right a) c = c a

inputFromShell :: Shell -> ProgramCont st (Shell, i) o m i r
inputFromShell shell cont =
  $waitInputDefC $ \(s, i) ->
    if s == shell
      then cont i
      else inputFromShell shell cont

getEnv :: Shell -> ProgramCont st (Shell, Input) (Shell, Text) m [(Text, Text)] r
getEnv shell cont =
  runCmd shell "env\n" $ \i ->
    finishOnErr
      (parseEnv i)
      cont

getCwd :: Shell -> ProgramCont st (Shell, Input) (Shell, Text) m Text r
getCwd shell cont =
  runCmd shell "pwd\n" (cont . T.strip)

stripAnsiEscapes :: Text -> Text
stripAnsiEscapes x = x

accumUntillPrompt :: Shell -> ProgramCont st (Shell, Input) o m Text r
accumUntillPrompt shell cont = loop []
  where
    loop res =
      inputFromShell shell $ \case
        Prompt -> cont (T.concat . reverse $ res)
        TextInput x -> loop (x : res)

expect :: Shell -> Text -> ProgramCont_ st (Shell, Input) m o r
expect shell exp cont =
  accumUntillPrompt shell $ \str ->
    if stripAnsiEscapes str == exp
      then cont
      else Finish . Left $ "expectation failed: " <> str <> " /= " <> exp

runCmdNoOut :: Shell -> Text -> ProgramCont_ st (Shell, Input) (Shell, Text) m r
runCmdNoOut shell cmd cont =
  Output (shell, cmd) $
    expect shell (cmd) cont

runCmd :: Shell -> Text -> ProgramCont st (Shell, Input) (Shell, Text) m Text r
runCmd shell cmd cont =
  Output (shell, cmd) $
    accumUntillPrompt shell $ \str ->
      let (a, b) = T.breakOn "\n" str
       in if T.null a
            then Finish . Left $ "expectation failed: empty output"
            else
              if a <> "\n" == cmd
                then (cont (T.drop 1 b))
                else Finish . Left $ ("expectation failed: " <> a <> "\n /= " <> cmd)

setEnv :: Shell -> [(Text, Text)] -> ProgramCont_ st (Shell, Input) (Shell, Text) m r
setEnv _ [] cont = cont
setEnv shell ((a, b) : es) cont =
  runCmdNoOut shell ("export " <> a <> "=" <> b <> "\n") $
    setEnv shell es cont

setCwd :: Shell -> Text -> ProgramCont_ st (Shell, Input) (Shell, Text) m r
setCwd shell cwd cont =
  runCmdNoOut shell ("cd '" <> cwd <> "'\n") cont

syncEnv :: Program st (Shell, Input) (Shell, Text) m ()
syncEnv = getEnv Shell_1 $ \env ->
  getCwd Shell_1 $ \cwd ->
    setEnv Shell_2 env $
      setCwd Shell_2 cwd $
        Finish (Right ())

data EvalState = EvalState
  { _pendingInputs :: [(Shell, Input)],
    _responses :: Map (Shell, Text) Text,
    _inputLog :: [(Shell, Input)]
  }
  deriving (Show)

showInputLog :: [(Shell, Input)] -> Text
showInputLog log =
  let f (Shell_1, x) = Left x
      f (Shell_2, x) = Right x
      g (TextInput x) = x
      g Prompt = " $ "
      go :: [Input] -> Text
      go = mconcat . fmap g
      (a, b) = partitionEithers (f <$> log)
   in "--- Shell_1 ---\n $ " <> go a <> "\n" <> "--- Shell_2 ---\n $ " <> go b

$(makeLenses 'EvalState)

main :: IO ()
main = either print putStrLn simulateEnvSync

defTime :: UTCTime
defTime = UTCTime (fromGregorian 2019 9 1) (timeOfDayToTime (TimeOfDay 15 13 0))

simulateEnvSync :: Either Text Text
simulateEnvSync =
  simulate
    (StepEnv defTime)
    (() :!: syncEnv)
    [ (Shell_1, "env\n", "a=1\nb=2\n"),
      (Shell_1, "pwd\n", "/root\n"),
      (Shell_2, "export a=1\n", ""),
      (Shell_2, "export b=2\n", ""),
      (Shell_2, "cd '/root'\n", "")
    ]

-- A simulator to play with our little DSL. Simulator expects program to send
-- commands with a single Output command. We have convention that all commands
-- and expectations end with "\n". Simulator responses are also supposed to end
-- with "\n".
-- We also expect that shell prompt is detected for us and stripped from input.
-- So we expect this:
--  $ ls
-- main.cpp main.o
--  $ rm main.o
-- To be represented like this (with variations on how input is split).
-- Prompt : Input "ls\nmain.cpp main.o\n" : Prompt : Input "rm main.o\n" : []
-- But we also expect that input can be split, so
-- Input "ls\nmain.cpp main.o\n" can be sent as Input "ls\n" : Input "main.cpp main.o\n"
-- or in other combinations.
simulate ::
  StepEnv ->
  Pair st (Program st (Shell, Input) (Shell, Text) (StateT EvalState (Except Text)) ()) ->
  [(Shell, Text, Text)] ->
  Either Text Text
simulate env p0 resp0 = getLog $ runProgram p0 resp0
  where
    getLog :: Either Text (Pair st (Either Text a), EvalState) -> Either Text Text
    getLog (Left err) = Left err
    getLog (Right (_ :!: Left err, _)) = Left err
    getLog (Right (_, st)) = Right $ showInputLog (st ^. inputLog)

    arrange :: (Shell, Text, Text) -> ((Shell, Text), Text)
    arrange (a, b, c) = ((a, b), c)

    runProgram ::
      Pair st (Program st (Shell, Input) (Shell, Text) (StateT EvalState (Except Text)) ()) ->
      [(Shell, Text, Text)] ->
      Either Text (Pair st (Either Text ()), EvalState)
    runProgram p resp =
      runIdentity $
        runExceptT $
          flip runStateT (EvalState [] (M.fromList (arrange <$> resp)) []) $
            evalProgramM
              env
              onOut
              getInput
              p

    onOut :: (Shell, Text) -> StateT EvalState (ExceptT Text Identity) ()
    onOut o@(s, cmd) = do
      use (responses . at o) >>= \case
        Nothing -> throwError ("onOut: no action defined for a command " <> show o)
        Just resp -> do
          let f =
                ( ++
                    [ (s, TextInput cmd),
                      (s, TextInput resp),
                      (s, Prompt)
                    ]
                )
          pendingInputs %= f
          inputLog %= f

    getInput :: StateT EvalState (ExceptT Text Identity) (Shell, Input)
    getInput =
      use pendingInputs >>= \case
        [] -> throwError "getInput: no more inputs"
        (x : xs) -> do
          pendingInputs .= xs
          pure x
