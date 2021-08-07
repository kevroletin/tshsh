{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}

module Spec.Muxer where

import Control.Lens hiding (_1', _2')
import Lang.Coroutine.CPS
import Spec.CPS.Folds
import Protolude
import Test.Hspec
import Test.Hspec.Core.Spec
import Data.Strict.Tuple
import Tshsh.Muxer.Body
import Data.BufferSlice (BufferSlice)
import qualified Data.BufferSlice as BufferSlice
import Matcher.ByteString

data TestSeq a = LeftBracket Int
               | RightBracket Int
               | TestData a
               deriving (Eq, Show, Functor)

mappendData :: Semigroup a => [TestSeq a] -> [TestSeq a]
mappendData [] = []
mappendData (TestData x : TestData y : rest) = mappendData (TestData (x <> y) : rest)
mappendData (x : xs) = x : mappendData xs

instance RaceMatchersCfg (TestSeq BufferSlice) where
  onData = TestData
  onFstEv = LeftBracket
  onSndEv = RightBracket

runProgram :: [inp] -> Pair st (Program st inp out Identity) -> [out]
runProgram inp = loop inp []
  where loop [] res _ = res
        loop (x:xs) res p =
          let
            (out, Cont p') = runIdentity $
              feedInputAccumOutputs
                x
                p
          in loop xs (res ++ out) p'

spec :: SpecM () ()
spec = do
  let st = mkSeqMatcher "<<" :!: mkSeqMatcher ">>"
  let beautify xs = mappendData $ (BufferSlice.sliceToByteString <$>) <$> xs
  it "no match" $ do
    let res = runProgram ["abc"] (st :!: raceMatchersP @(TestSeq BufferSlice))
    beautify res `shouldBe` [TestData "abc"]

  it "fst match" $ do
    let res = runProgram ["a>>b"] (st :!: raceMatchersP @(TestSeq BufferSlice))
    beautify res `shouldBe` [TestData "a>>", RightBracket 2, TestData "b"]

  it "snd match" $ do
    let res = runProgram ["a<<b"] (st :!: raceMatchersP @(TestSeq BufferSlice))
    beautify res `shouldBe` [TestData "a<<", LeftBracket 2, TestData "b"]

  it "fst match split" $ do
    let res = runProgram ["a>", ">b"] (st :!: raceMatchersP @(TestSeq BufferSlice))
    beautify res `shouldBe` [TestData "a>>", RightBracket 2, TestData "b"]

  it "snd match split" $ do
    let res = runProgram ["a<", "<b"] (st :!: raceMatchersP @(TestSeq BufferSlice))
    beautify res `shouldBe` [TestData "a<<", LeftBracket 2, TestData "b"]

  it "snd after fst" $ do
    let res = runProgram ["a>", ">b<", "<c"] (st :!: raceMatchersP @(TestSeq BufferSlice))
    beautify res `shouldBe` [TestData "a>>", RightBracket 2, TestData "b<<", LeftBracket 2, TestData "c"]

  it "fst after snd" $ do
    let res = runProgram ["a<", "<b>", ">c"] (st :!: raceMatchersP @(TestSeq BufferSlice))
    beautify res `shouldBe` [TestData "a<<", LeftBracket 2, TestData "b>>", RightBracket 2, TestData "c"]

  it "no spaces in between" $ do
    let res = runProgram ["<", "<", ">", ">", "<", "<", ">", ">"] (st :!: raceMatchersP @(TestSeq BufferSlice))
    beautify res `shouldBe` [ TestData "<<", LeftBracket 2, TestData ">>", RightBracket 2,
                              TestData "<<", LeftBracket 2, TestData ">>", RightBracket 2]

  it "partial matches" $ do
    let res = runProgram ["<", ">", "<", ">", "<", "<", ">", ">"] (st :!: raceMatchersP @(TestSeq BufferSlice))
    beautify res `shouldBe` [ TestData "<><><<", LeftBracket 2, TestData ">>", RightBracket 2]
