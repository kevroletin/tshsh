{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Spec.MonadT where

import Control.Lens
import Lang.Coroutine.MonadT
import Lang.Coroutine.MonadT.Folds
import Protolude
import Test.Hspec
import Test.Hspec.Core.Spec
import Test.Hspec.Expectations.Lens

accumOutputs :: forall i o r f.
  Applicative f =>
  Program i o () (StateT [o] Identity) r ->
  f ([o], Res i o (StateT [o] Identity) r)
accumOutputs p = do
  let (res, out) =
        runState
          (eatOutputsM (modify . (:)) p)
          []
  pure (reverse out, res)

feedInputAccumOutputs ::
  i ->
  Program i o () (State [o]) r ->
  ([o], Res i o (State [o]) r)
feedInputAccumOutputs i p =
  let (res, out) =
        runState
          (feedInputM (modify . (:)) i p)
          []
  in (reverse out, res)

accumProgram :: forall i o r.
  [i] ->
  Program i o () (StateT ([o], [i]) (Except Text)) r ->
  Either Text ([o], r)
accumProgram is0 p =
  let res =
        runExcept $
          runStateT
            (evalProgramM
              (\o -> do (os, is) <- get
                        put (o:os, is)
                        pure ())
              (do (o, is) <- get
                  case is of
                    [] -> throwError "accumProgram: end of input"
                    (x:xs) -> do
                      put (o, xs)
                      pure x
              )
              p
            )
         ([], is0)
  in case res of
       Left err -> Left err
       Right (Left err, _) -> Left err
       Right (Right x, (out, _)) -> Right (reverse out, x)

spec :: SpecM () ()
spec = do
  it "finish x == x" $ do
    (out, res) <- accumOutputs (Finish (Right (10 :: Int)))

    out `shouldBe` ([] :: [Int])
    res `shouldHave` _Res . _Right . only 10

  it "andThen(nop, finish x) == x" $ do
    (out, res) <-
          accumOutputs @Int @Int @Int
            ( AndThen
                (Finish (Right ()))
                (Finish (Right 10))
            )
    out `shouldBe` []
    res `shouldHave` _Res . _Right . only 10

  it "lam should pass value" $ do
    (out, res) <-
          accumOutputs @Int @Int @Int
            ( AndThen
                (Finish (Right 10))
                (Lam $ \s -> Finish (Right s))
            )
    out `shouldBe` ([] :: [Int])
    res `shouldHave` _Res . _Right . only 10

  it "output outputs" $ do
    (out, res) <-
          accumOutputs @Int @Int @Int
            ( do Output 1
                 Output 2
                 Output 3
                 Finish (Right 4)
            )
    out `shouldBe` ([1, 2, 3])
    res `shouldHave` _Res . _Right . only 4

  it "waitInput doesn't output" $ do
    (out, res) <-
          accumOutputs @Int @Int
            (do i <- WaitInput
                Output i
                Finish (Right ()))
    out `shouldBe` []
    res `shouldHave` _Cont

  it "program executes until the frist waitInput" $ do
    (out, res) <-
          accumOutputs @Int @Int
            (do Output 1
                i <- WaitInput
                Output i
                (Finish (Right ())))
    out `shouldBe` [1]
    res `shouldHave` _Cont

  it "feedInput" $ do
    let (out, res) =
          feedInputAccumOutputs @Int @Int
            10
            (do Output 1
                i <- WaitInput
                Output i
                Finish (Right ()))
    out `shouldBe` [1, 10]
    res `shouldHave` _Res . _Right . only ()

  it "program can have recursion" $ do
    let loop = do i <- WaitInput
                  Output i
                  loop

    let res = accumProgram @Int @Int @() [1, 2, 3 :: Int] loop
    res `shouldBe` Left "accumProgram: end of input"

  it "stateful program" $ do
    let loop 0 = Finish (Right ())
        loop n = do i <- WaitInput
                    Output i
                    loop (n -1)

    accumProgram @Int @Int @() [1 ..] (loop (10 :: Int))
      `shouldBe` Right ([1 .. 10], ())

  let echo = do i <- WaitInput
                Output i
                Finish (Right ())

  it "(andThen a b) terminates" $ do
    let p = do echo
               echo

    accumProgram @Int @Int @() [1, 2, 3 :: Int] p
      `shouldBe` Right ([1, 2], ())

  it "andThen (andThen a b) c terminates" $ do
    let p = do do echo
                  echo
               echo

    accumProgram @Int @Int @() [1, 2, 3 :: Int] p
      `shouldBe` Right ([1, 2, 3], ())

  it "andThen a (andThen a c) terminates" $ do
    let p = do echo
               do echo
                  echo

    accumProgram @Int @Int @() [1, 2, 3 :: Int] p
      `shouldBe` Right ([1, 2, 3], ())
-- TODO: feeding input without consuming output throws errors
