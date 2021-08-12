{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}

module Spec.CPS where

import Control.Lens
import Lang.Coroutine.CPS
import Spec.CPS.Folds
import Protolude
import Test.Hspec
import Test.Hspec.Core.Spec
import Test.Hspec.Expectations.Lens
import Data.Strict.Tuple.Extended

rid :: Identity a -> a
rid = runIdentity

spec :: SpecM () ()
spec = do
  it "finish x == x" $ do
    let (out, res) = rid $ accumOutputs @() @Int @Int (() :!: Finish (Right ()))
    out `shouldBe` []
    res `shouldHave` _Res . _2 . _Right

  it "output outputs" $ do
    let (out, res) =
          rid $ accumOutputs @() @Int @Int
            ( () :!:
              (Output 1 $
                Output 2 $
                  Output 3 $
                    Finish (Right ()))
            )
    out `shouldBe` [1, 2, 3]
    res `shouldHave` _Res . _2 . _Right

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
    res `shouldHave` _Res . _2 . _Right . only ()

  it "program can have recursion" $ do
    let loop = WaitInput $ \i -> Output i loop
        (out, cont) = rid $ accumProgram @() @Int @Int [1, 2, 3 :: Int] (() :!: loop)
    out `shouldBe` [1, 2, 3]
    cont `shouldHave` _Cont

  it "consumes all input" $ do
    let loop 0 = Finish (Right ())
        loop n = WaitInput $ \i -> Output i (loop (n -1))

        (out, res) = rid $ accumProgram @() @Int @Int [1 ..] (() :!: loop (10 :: Int))
    out `shouldBe` [1 .. 10]
    res `shouldHave` _Res

  it "can access the state before program terminates" $ do
    let sumLoop = WaitInput $ \i ->
                  GetState $ \st ->
                  PutState (i + st) $
                  sumLoop

        (out, res) = rid $ accumProgram @Int @Int @Int [1..10] (0 :!: sumLoop)
    out `shouldBe` []
    res `shouldHave` _Cont . _1 . only (55 :: Int)

  it "can access the state after program terminates" $ do
    let sumLoop = WaitInput $ \i ->
                  GetState $ \st ->
                  PutState (i + st) $
                  if (i + st) < 100
                    then sumLoop
                    else Output i $ Finish (Right ())

        (out, res) = rid $ accumProgram @Int @Int @Int [1..] (0 :!: sumLoop)
    out `shouldBe` [14]
    res `shouldHave` _Res . only (105 :!: Right ()) -- sum [1..14] == 105

  it "lift" $ do
    let sumLoop = WaitInput $ \i ->
                  Lift (modify (+i)) $ \() ->
                  Output i
                  sumLoop

    let ((out, res), st)  = flip runState 0 $ accumProgram @Int @Int @Int [1..10] (0 :!: sumLoop)

    st `shouldBe` 55
    out `shouldBe` [1..10]
    res `shouldHave` _Cont

  -- TODO: this test doesn't check that we consume all outputs
  -- in a single consumOutputs call. We saw in practice that because
  -- feedInputM calls consumeOutputs first, it masks the problem
  -- when incorrect implementation of `step Pipe` retains some outputs
  it "pipe" $ do
    let groupInp n = GetState $ \(acc, _) ->
          if length acc < n
            then
              WaitInput $ \i ->
                ModifyState (_1 %~ (++ [i]))
                  (groupInp n)
            else
              ModifyState (_1 .~ []) $
                Output acc (groupInp n)
    let sumInp = WaitInput $ \i -> Output (sum i) sumInp
    let takeNInp n = GetState $ \(_, t) ->
          if t < n
            then WaitInput $ \i ->
              ModifyState (_2 %~ (+ 1)) $
                Output i (takeNInp n)
            else Finish (Right ())
    let p = groupInp 3 `Pipe` sumInp `Pipe` takeNInp 10
    let (out, _) =
          rid $
            accumProgram @([Int], Int) @Int @Int @_
              [1 ..]
              (([], 0) :!: p)
    out `shouldBe` [6, 15, 24, 33, 42, 51, 60, 69, 78, 87]

  let echo = WaitInput $ \i -> Output i finishP

  it "(andThen a b) terminates" $ do
    let p = AndThen echo echo

    let (out, _) = rid (accumProgram @() @Int @Int [1, 2, 3 :: Int] (() :!: p))
    out `shouldBe` [1, 2]

  it "andThen (andThen a b) c terminates" $ do
    let p = AndThen (AndThen echo echo) echo

    let (out, _) = rid (accumProgram @() @Int @Int [1, 2, 3 :: Int] (() :!: p))
    out `shouldBe` [1, 2, 3]

  it "andThen a (andThen a c) terminates" $ do
    let p = AndThen echo (AndThen echo echo)

    let (out, _) = rid (accumProgram @() @Int @Int [1, 2, 3 :: Int] (() :!: p))
    out `shouldBe` [1, 2, 3]
