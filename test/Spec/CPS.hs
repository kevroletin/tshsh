{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Spec.CPS where

import Control.Lens
import Data.Strict.Tuple.Extended
import Protolude
import Spec.CPS.Folds
import Test.Hspec
import Test.Hspec.Core.Spec
import Test.Hspec.Expectations.Lens
import Tshsh.Lang.Coroutine.CPS

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
          rid $
            accumOutputs @() @Int @Int
              ( ()
                  :!: ( Output 1 $
                          Output 2 $
                            Output 3 $
                              Finish (Right ())
                      )
              )
    out `shouldBe` [1, 2, 3]
    res `shouldHave` _Res . _2 . _Right

  it "waitInput doesn't output" $ do
    let (out, res) =
          rid $
            accumOutputs @() @Int @Int
              (() :!: (WaitInput $ \i -> Output i (Finish (Right ()))))
    out `shouldBe` []
    res `shouldHave` _Cont

  it "program executes until the frist waitInput" $ do
    let (out, res) =
          rid $
            accumOutputs @() @Int @Int
              (() :!: (Output 1 $ WaitInput $ \i -> Output i (Finish (Right ()))))
    out `shouldBe` [1]
    res `shouldHave` _Cont

  it "feedInput" $ do
    let (out, res) =
          rid $
            feedInputAccumOutputs @() @Int @Int
              10
              (() :!: (Output 1 $ WaitInput $ \i -> Output i (Finish (Right ()))))
    out `shouldBe` [1, 10]
    res `shouldHave` _Res . _2 . _Right . only ()

  it "program can have recursion" $ do
    let loop = WaitInput $ \i -> Output i loop
        (out, cont) = rid $ accumProgram @() @Int @Int @_ @() [1, 2, 3 :: Int] (() :!: loop)
    out `shouldBe` [1, 2, 3]
    cont `shouldHave` _Cont

  it "consumes all input" $ do
    let loop 0 = Finish (Right ())
        loop n = WaitInput $ \i -> Output i (loop (n -1))

        (out, res) = rid $ accumProgram @() @Int @Int @_ @() [1 ..] (() :!: loop (10 :: Int))
    out `shouldBe` [1 .. 10]
    res `shouldHave` _Res

  it "can access the state before program terminates" $ do
    let sumLoop = WaitInput $ \i ->
          GetState $ \st ->
            PutState (i + st) $
              sumLoop

        (out, res) = rid $ accumProgram @Int @Int @Int @_ @() [1 .. 10] (0 :!: sumLoop)
    out `shouldBe` []
    res `shouldHave` _Cont . _1 . only (55 :: Int)

  it "can access the state after program terminates" $ do
    let sumLoop = WaitInput $ \i ->
          GetState $ \st ->
            PutState (i + st) $
              if (i + st) < 100
                then sumLoop
                else Output i $ Finish (Right ())

        (out, res) = rid $ accumProgram @Int @Int @Int [1 ..] (0 :!: sumLoop)
    out `shouldBe` [14]
    res `shouldHave` _Res . only (105 :!: Right ()) -- sum [1..14] == 105
  it "lift" $ do
    let sumLoop = WaitInput $ \i ->
          Lift (modify (+ i)) $ \() ->
            Output
              i
              sumLoop

    let ((out, res), st) = flip runState 0 $ accumProgram @Int @Int @Int @_ @() [1 .. 10] (0 :!: sumLoop)

    st `shouldBe` 55
    out `shouldBe` [1 .. 10]
    res `shouldHave` _Cont

  -- TODO: this test doesn't check that we consume all outputs
  -- in a single consumOutputs call. We saw in practice that because
  -- feedInputM calls consumeOutputs first, it masks the problem
  -- when incorrect implementation of `step Pipe` retains some outputs
  it "pipe" $ do
    let groupInp n = GetState $ \(acc, _) ->
          if length acc < n
            then WaitInput $ \i ->
              ModifyState
                (_1 %~ (++ [i]))
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
    let p = groupInp 3 `pipe` sumInp `pipe` takeNInp 10
    let (out, _) =
          rid $
            accumProgram @([Int], Int) @Int @Int @_
              [1 ..]
              (([], 0) :!: p)
    out `shouldBe` [6, 15, 24, 33, 42, 51, 60, 69, 78, 87]

  it "Long pipe" $ do
    let addPrefix :: Text -> Program () Text Text Identity ()
        addPrefix msg =
          WaitInput $ \i ->
            Output (msg <> i) (addPrefix msg)

    let dup =
          WaitInput $ \i ->
            Output i $
              Output i dup

    let p = addPrefix ">" `pipe` dup `pipe` addPrefix "<" `pipe` dup
    let (out, _) =
          rid $
            accumProgram @() @Text @Text @_
              (["1", "2"])
              (() :!: p)
    out
      `shouldBe` [ "<>1",
                   "<>1",
                   "<>1",
                   "<>1",
                   "<>2",
                   "<>2",
                   "<>2",
                   "<>2"
                 ]

  let echo = WaitInput $ \i -> Output i finishP_

  it "(andThen a b) terminates" $ do
    let p = andThenP_ echo echo

    let (out, _) = rid (accumProgram @() @Int @Int [1, 2, 3 :: Int] (() :!: p))
    out `shouldBe` [1, 2]

  it "andThen (andThen a b) c terminates" $ do
    let p = andThenP_ (andThenP_ echo echo) echo

    let (out, _) = rid (accumProgram @() @Int @Int [1, 2, 3 :: Int] (() :!: p))
    out `shouldBe` [1, 2, 3]

  it "andThen a (andThen a c) terminates" $ do
    let p = andThenP_ echo (andThenP_ echo echo)

    let (out, _) = rid (accumProgram @() @Int @Int [1, 2, 3] (() :!: p))
    out `shouldBe` [1, 2, 3]

  it "andThen passes result" $ do
    let p =
          (Output 1 $ finishP "a") `AndThen` \a ->
            (Output 2 $ finishP (a <> "b")) `AndThen` \b ->
              (Output 3 $ finishP (b <> "c"))

    let (out, res) = rid (accumProgram @() @Int @Int @_ @Text [] (() :!: p))
    out `shouldBe` [1, 2, 3]
    res `shouldHave` _Res . only (() :!: Right "abc")

  it "Lift passes result" $ do
    let p = Lift (pure 123) finishP

    let (out, res) = rid (accumProgram @() @() @() @_ @Int [] (() :!: p))
    out `shouldBe` []
    res `shouldHave` _Res . only (() :!: Right 123)

  it "Pipe passes result (no input)" $ do
    let producer n = Output n (producer (n + 1))
        consumer = WaitInput finishP
        p = producer 0 `pipe` consumer

    let (out, res) = rid (accumProgram @() @() @() @_ @Int [] (() :!: p))
    out `shouldBe` []
    res `shouldHave` _Res . only (() :!: Right 0)

  it "Pipe passes result (with input)" $ do
    let consumer n =
          WaitInput $ \i ->
            if i < n
              then Output i (consumer n)
              else finishP n
        echoLoop = WaitInput $ \i -> Output i echoLoop
        p = echoLoop `pipe` consumer 5

    let (out, res) = rid (accumProgram @() @Int @Int @_ @Int [1 .. 100] (() :!: p))
    out `shouldBe` [1, 2, 3, 4]
    res `shouldHave` _Res . only (() :!: Right 5)
