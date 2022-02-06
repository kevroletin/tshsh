{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spec.Muxer where

import Data.Time
import Control.Lens
import Data.Strict.Tuple
import Protolude
import Spec.CPS.Folds
import Test.Hspec
import Test.Hspec.Core.Spec
import Tshsh.Data.BufferSlice (BufferSlice)
import qualified Tshsh.Data.BufferSlice as BufferSlice
import Tshsh.Lang.Coroutine.CPS
import Tshsh.Matcher
import Tshsh.Muxer.ShellOutputParser

data TestSeq a
  = LeftBracket Int
  | RightBracket Int
  | TestData a
  deriving (Eq, Show, Functor)

instance RaceMatchersDataCfg (TestSeq BufferSlice) where
  type FstParam (TestSeq BufferSlice) = Int
  type SndParam (TestSeq BufferSlice) = Int
  onData = TestData
  onFstEv = LeftBracket
  onSndEv = RightBracket

instance RaceMatchersStateCfg (Pair (StreamConsumer ByteString Int) (StreamConsumer ByteString Int)) (TestSeq BufferSlice) where
  fstMatcher f (a :!: b) = (:!: b) <$> f a
  sndMatcher f (a :!: b) = (a :!:) <$> f b

mappendData :: Semigroup a => [TestSeq a] -> [TestSeq a]
mappendData [] = []
mappendData (TestData x : TestData y : rest) = mappendData (TestData (x <> y) : rest)
mappendData (x : xs) = x : mappendData xs

runProgram :: StepEnv -> [inp] -> Pair st (Program st inp out Identity r) -> [out]
runProgram env inp = loop inp []
  where
    loop [] res _ = res
    loop (x : xs) res p =
      let (out, Cont (st :!: p')) =
            runIdentity $
              feedInputAccumOutputs
                env
                x
                p
       in loop xs (res ++ out) (st :!: unProgramEv p')

defTime :: UTCTime
defTime = UTCTime (fromGregorian 2019 9 1) (timeOfDayToTime (TimeOfDay 15 13 0))

defEnv :: StepEnv
defEnv = StepEnv defTime

spec :: SpecM () ()
spec = do
  let st :: Pair (StreamConsumer ByteString Int) (StreamConsumer ByteString Int)
      st = mkSeqMatcher "<<" :!: mkSeqMatcher ">>"
  let beautify xs = mappendData $ (BufferSlice.sliceToByteString <$>) <$> xs
  it "no match" $ do
    let res = runProgram defEnv ["abc" :: BufferSlice] (st :!: raceMatchersP @_ @(TestSeq BufferSlice))
    beautify res `shouldBe` [TestData "abc"]

  it "fst match" $ do
    let res = runProgram defEnv ["a>>b"] (st :!: raceMatchersP @_ @(TestSeq BufferSlice))
    beautify res `shouldBe` [TestData "a>>", RightBracket 2, TestData "b"]

  it "snd match" $ do
    let res = runProgram defEnv ["a<<b"] (st :!: raceMatchersP @_ @(TestSeq BufferSlice))
    beautify res `shouldBe` [TestData "a<<", LeftBracket 2, TestData "b"]

  it "fst match split" $ do
    let res = runProgram defEnv ["a>", ">b"] (st :!: raceMatchersP @_ @(TestSeq BufferSlice))
    beautify res `shouldBe` [TestData "a>>", RightBracket 2, TestData "b"]

  it "snd match split" $ do
    let res = runProgram defEnv ["a<", "<b"] (st :!: raceMatchersP @_ @(TestSeq BufferSlice))
    beautify res `shouldBe` [TestData "a<<", LeftBracket 2, TestData "b"]

  it "snd after fst" $ do
    let res = runProgram defEnv ["a>", ">b<", "<c"] (st :!: raceMatchersP @_ @(TestSeq BufferSlice))
    beautify res `shouldBe` [TestData "a>>", RightBracket 2, TestData "b<<", LeftBracket 2, TestData "c"]

  it "fst after snd" $ do
    let res = runProgram defEnv ["a<", "<b>", ">c"] (st :!: raceMatchersP @_ @(TestSeq BufferSlice))
    beautify res `shouldBe` [TestData "a<<", LeftBracket 2, TestData "b>>", RightBracket 2, TestData "c"]

  it "no spaces in between" $ do
    let res = runProgram defEnv ["<", "<", ">", ">", "<", "<", ">", ">"] (st :!: raceMatchersP @_ @(TestSeq BufferSlice))
    beautify res
      `shouldBe` [ TestData "<<",
                   LeftBracket 2,
                   TestData ">>",
                   RightBracket 2,
                   TestData "<<",
                   LeftBracket 2,
                   TestData ">>",
                   RightBracket 2
                 ]

  it "partial matches" $ do
    let res = runProgram defEnv ["<", ">", "<", ">", "<", "<", ">", ">"] (st :!: raceMatchersP @_ @(TestSeq BufferSlice))
    beautify res `shouldBe` [TestData "<><><<", LeftBracket 2, TestData ">>", RightBracket 2]
