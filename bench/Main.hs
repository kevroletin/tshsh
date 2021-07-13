{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

import System.Environment
import qualified Lang.Final.Simple
import qualified Lang.Final.NoData
import qualified Lang.Final.Zipper
import qualified Lang.Initial.Zipper
import qualified Lang.Initial.Simple

n = 1_000

main :: IO ()
main = do
  tail <$> getArgs >>= \case
    ("Lang.Final.Simple.test_leftCnt":_) -> print $ Lang.Final.Simple.test_leftCnt n
    ("Lang.Final.Simple.test_rightCnt":_) -> print $ Lang.Final.Simple.test_rightCnt n
    ("Lang.Final.NoData.test_leftCnt":_) -> print $ Lang.Final.NoData.test_leftCnt n
    ("Lang.Final.NoData.test_rightCnt":_) -> print $ Lang.Final.NoData.test_rightCnt n
    ("Lang.Final.Zipper.test_leftCnt":_) -> print $ Lang.Final.Zipper.test_leftCnt n
    ("Lang.Final.Zipper.test_rightCnt":_) -> print $ Lang.Final.Zipper.test_rightCnt n
    ("Lang.Initial.Zipper.test_leftCnt":_) -> print $ Lang.Initial.Zipper.test_leftCnt n
    ("Lang.Initial.Zipper.test_rightCnt":_) -> print $ Lang.Initial.Zipper.test_rightCnt n
    ("Lang.Initial.Simple.test_leftCnt":_) -> print $ Lang.Initial.Simple.test_leftCnt n
    ("Lang.Initial.Simple.test_rightCnt":_) -> print $ Lang.Initial.Simple.test_rightCnt n
