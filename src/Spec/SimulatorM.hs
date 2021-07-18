{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.SimulatorM where

import Control.Lens
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import Lang.Coroutine.MonadT.Folds
import Lang.Coroutine.MonadT
import Protolude hiding ((>>), (>>=), exp, log)

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

finishOnErr :: Either Text t -> Program i o () m t
finishOnErr = Finish

inputFromShell :: Shell -> Program (Shell, i) o () m i
inputFromShell shell = do
  let loop = do (s, i) <- WaitInput
                if s == shell
                  then Finish (Right i)
                  else loop
  loop

getEnv :: Shell -> Program (Shell, Input) (Shell, Text) () m [(Text, Text)]
getEnv shell = do
  i <- runCmd shell "env\n"
  finishOnErr (parseEnv i)

getCwd :: Shell -> Program (Shell, Input) (Shell, Text) () m Text
getCwd shell =
  T.strip <$> runCmd shell "pwd\n"

stripAnsiEscapes :: Text -> Text
stripAnsiEscapes x = x

accumUntillPrompt :: Shell -> Program (Shell, Input) o () m Text
accumUntillPrompt shell = do
  let loop res =
        inputFromShell shell >>= \case
           Prompt -> pure (T.concat . reverse $ res)
           TextInput x -> loop (x : res)
  loop []

expect :: Shell -> Text -> Program (Shell, Input) o () m ()
expect shell exp = do
  str <- accumUntillPrompt shell
  if stripAnsiEscapes str == exp
    then pure ()
    else Finish . Left $ "expectation failed: " <> str <> " /= " <> exp

runCmdNoOut :: Shell -> Text -> Program (Shell, Input) (Shell, Text) () m ()
runCmdNoOut shell cmd = do
  Output (shell, cmd)
  expect shell (cmd)

runCmd :: Shell -> Text -> Program (Shell, Input) (Shell, Text) () m Text
runCmd shell cmd = do
  Output (shell, cmd)
  str <- accumUntillPrompt shell
  let (a, b) = T.breakOn "\n" str
  if T.null a
    then Finish . Left $ "expectation failed: empty output"
    else
      if a <> "\n" == cmd
        then pure (T.drop 1 b)
        else Finish . Left $ ("expectation failed: " <> a <> "\n /= " <> cmd)

setEnv :: Shell -> [(Text, Text)] -> Program (Shell, Input) (Shell, Text) () m ()
setEnv _ [] = pure ()
setEnv shell ((a, b) : es) = do
  runCmdNoOut shell ("export " <> a <> "=" <> b <> "\n")
  setEnv shell es

setCwd :: Shell -> Text -> Program (Shell, Input) (Shell, Text) () m ()
setCwd shell cwd =
  runCmdNoOut shell ("cd '" <> cwd <> "'\n")

andThen :: Program i o s m b -> (b -> Program i o () m t) -> Program i o s m t
andThen a b = AndThen a (Lam b)

syncEnv :: Program (Shell, Input) (Shell, Text) () m ()
syncEnv = do
  env <- getEnv Shell_1
  cwd <- getCwd Shell_1
  setEnv Shell_2 env
  setCwd Shell_2 cwd
  pure ()

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

simulateEnvSync :: Either Text Text
simulateEnvSync =
  simulate
    syncEnv
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
  Program (Shell, Input) (Shell, Text) () (StateT EvalState (ExceptT Text Identity)) r ->
  [(Shell, Text, Text)] ->
  Either Text Text
simulate p0 resp0 = getLog $ runProgram p0 resp0
  where
    getLog :: Either Text (Either Text a, EvalState) -> Either Text Text
    getLog (Left err) = Left err
    getLog (Right (Left err, _)) = Left err
    getLog (Right (_, st)) = Right $ showInputLog (st ^. inputLog)

    arrange :: (Shell, Text, Text) -> ((Shell, Text), Text)
    arrange (a, b, c) = ((a, b), c)

    runProgram ::
      Program (Shell, Input) (Shell, Text) () (StateT EvalState (ExceptT Text Identity)) r ->
      [(Shell, Text, Text)] ->
      Either Text (Either Text r, EvalState)
    runProgram p resp =
      runIdentity $
        runExceptT $
          flip runStateT (EvalState [] (M.fromList (arrange <$> resp)) []) $
            evalProgramM
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
      use (pendingInputs) >>= \case
        [] -> throwError "getInput: no more inputs"
        (x : xs) -> do
          pendingInputs .= xs
          pure x
