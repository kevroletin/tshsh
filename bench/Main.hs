{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

import Control.Monad
import Control.Monad.Except
import Lang.Coroutine.Folds
import Lang.Coroutine.Program
import qualified Lang.Coroutine.Test as Test
import Protolude
import System.Environment
import System.Random

main :: IO ()
main = do
  res :: Either Text () <-
    (join <$>) $
      runExceptT $
        flip evalStateT [1, 2, 3, 4, 5 :: Int] $
          evalProgramM
            (liftIO . print)
            ( get >>= \case
                (x : xs) -> put xs >> pure x
                [] -> throwError "End of input"
            )
            Test.inputToOutputLoop
  print res
