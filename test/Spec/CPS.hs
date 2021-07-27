{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}

module Spec.CPS where

import Control.Lens hiding (_1', _2')
import Lang.Coroutine.CPS
import Spec.CPS.Folds
import Protolude
import Test.Hspec
import Test.Hspec.Core.Spec
import Test.Hspec.Expectations.Lens
import Data.Strict.Tuple

rid :: Identity a -> a
rid = runIdentity

_1' :: Lens' (Pair a b) a
_1' f (a :!: b) = (:!: b) <$> f a
_2' :: Lens' (Pair a b) b
_2' f (a :!: b) = (a :!:) <$> f b

spec :: SpecM () ()
spec = do
  it "finish x == x" $ do
    let (out, res) = rid $ accumOutputs @() @Int @Int (() :!: Finish (Right (10 :: Int)))
    out `shouldBe` []
    res `shouldHave` _Res . _2' . _Right . only 10

  it "output outputs" $ do
    let (out, res) =
          rid $ accumOutputs @() @Int @Int @Int
            ( () :!:
              (Output 1 $
                Output 2 $
                  Output 3 $
                    Finish (Right 4))
            )
    out `shouldBe` [1, 2, 3]
    res `shouldHave` _Res . _2' . _Right . only 4

  it "waitInput doesn't output" $ do
    let (out, res) =
          rid $ accumOutputs @() @Int @Int
            (() :!: (WaitInput $ \i -> Output i (Finish (Right ()))))
    out `shouldBe` []
    res `shouldHave` _Cont

  it "program executes until the frist waitInput" $ do
    let (out, res) =
          rid $ accumOutputs @() @Int @Int
            (() :!: (Output 1 $ WaitInput $ \i -> Output i (Finish (Right ()))))
    out `shouldBe` [1]
    res `shouldHave` _Cont

  it "feedInput" $ do
    let (out, res) =
          rid $ feedInputAccumOutputs @() @Int @Int
            10
            (() :!: (Output 1 $ WaitInput $ \i -> Output i (Finish (Right ()))))
    out `shouldBe` [1, 10]
    res `shouldHave` _Res . _2' . _Right . only ()

  it "program can have recursion" $ do
    let loop = WaitInput $ \i -> Output i loop
        (out, cont) = rid $ accumProgram @() @Int @Int @() [1, 2, 3 :: Int] (() :!: loop)
    out `shouldBe` [1, 2, 3]
    cont `shouldHave` _Cont

  it "consumes all input" $ do
    let loop 0 = Finish (Right ())
        loop n = WaitInput $ \i -> Output i (loop (n -1))

        (out, res) = rid $ accumProgram @() @Int @Int @() [1 ..] (() :!: loop (10 :: Int))
    out `shouldBe` [1 .. 10]
    res `shouldHave` _Res

  it "can access the state before program terminates" $ do
    let sumLoop = WaitInput $ \i ->
                  GetState $ \st ->
                  PutState (i + st) $
                  sumLoop

        (out, res) = rid $ accumProgram @Int @Int @Int @Int [1..10] (0 :!: sumLoop)
    out `shouldBe` []
    res `shouldHave` _Cont . _1' . only (55 :: Int)

  it "can access the state after program terminates" $ do
    let sumLoop = WaitInput $ \i ->
                  GetState $ \st ->
                  PutState (i + st) $
                  if (i + st) < 100
                    then sumLoop
                    else Finish (Right i)

        (out, res) = rid $ accumProgram @Int @Int @Int @Int [1..] (0 :!: sumLoop)
    out `shouldBe` []
    res `shouldHave` _Res . only (105 :!: Right 14) -- sum [1..14] == 105

  it "lift" $ do
    let sumLoop = WaitInput $ \i ->
                  Lift (modify (+i)) $ \() ->
                  Output i
                  sumLoop

    let ((out, res), st)  = flip runState 0 $ accumProgram @Int @Int @Int @Int [1..10] (0 :!: sumLoop)

    st `shouldBe` 55
    out `shouldBe` [1..10]
    res `shouldHave` _Cont
