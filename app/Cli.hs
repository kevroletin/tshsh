module Cli
  ( CliOptions (..),
    parseArgs,
  )
where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String.Conversions
import qualified Data.Text as T
import Protolude

data CliOptions = CliOptions
  { _cl_muxLog :: Maybe Text,
    _cl_log :: Maybe Text
  }
  deriving (Show)

parseArgs_ :: Set Text -> Map Text Text -> [Text] -> [Text] -> Either Text (Map Text Text, [Text])
parseArgs_ _ opts args [] = Right (opts, reverse args)
parseArgs_ knownOpts opts args (x1 : xs1)
  | T.isPrefixOf "--" x1 =
    case Set.member x1 knownOpts of
      False ->
        Left $ "unexpected option: " <> x1
      True ->
        case xs1 of
          (x2 : xs2) -> parseArgs_ knownOpts (Map.insert x1 x2 opts) args xs2
          [] -> Left $ "expecting a value after " <> x1 <> " option"
  | otherwise =
    parseArgs_ knownOpts opts (x1 : args) xs1

parseArgs :: IO (CliOptions, [Text])
parseArgs = do
  args0 <- getArgs
  let muxLogOpt = "--mux-log"
  let logOpt = "--log"
  case parseArgs_ (Set.fromList [muxLogOpt, logOpt]) Map.empty [] (fmap cs args0) of
    Left err -> do
      hPutStrLn stderr ("\nError: " <> err <> "\n")
      exitFailure
    Right (opts, args) ->
      pure (CliOptions (Map.lookup muxLogOpt opts) (Map.lookup logOpt opts), args)
