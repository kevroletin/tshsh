{-# LANGUAGE RebindableSyntax #-}

-- | Usage examples for AndThen and experements with do-notation
module Lang.Coroutine.Test where

import Lang.Coroutine.Folds
import Lang.Coroutine.Program
import Protolude
import System.Random (randomIO)

inputToOutputLoop :: Program a a () b
inputToOutputLoop = let loop = WaitInput (\x -> Output x loop) in loop

inputToOutput :: a -> [a] -> Program a a () r -> (a, Either Text r)
inputToOutput = foldProgram (\_ x -> x)

nop :: Program i o () ()
nop = Finish (Right ())

andThen_ :: Program i o a () -> Program i o () c -> Program i o a c
andThen_ a b = a `AndThen` Lam (const b)

testLeftInt :: Int -> Program Int Int () ()
testLeftInt 0 = nop
testLeftInt n0 =
  andThen_
    (testLeftInt (n0 -1))
    (WaitInput (\n -> Output (n + 1) nop))

testRightInt :: Int -> Program Int Int () ()
testRightInt 0 = nop
testRightInt n0 =
  andThen_
    (WaitInput (\n -> Output (n + 1) nop))
    (testRightInt (n0 -1))

test_leftCnt :: Int -> Int
test_leftCnt n = x where (x, _) = inputToOutput 0 [1 ..] (testLeftInt n)

test_rightCnt :: Int -> Int
test_rightCnt n = x where (x, _) = inputToOutput 0 [1 ..] (testRightInt n)

test_printLoop :: Int -> IO ()
test_printLoop n =
  void $ evalProgramM print ((`mod` 100) <$> randomIO :: IO Int) inputToOutputLoop

test :: Program Int Int () Int
test = do
  a <- WaitInput (Finish . Right)
  b <- WaitInput (Finish . Right)
  Output a
  Output b
  Finish (Right (a + b))
  where
    (>>=) a b = a `AndThen` Lam b
    (>>) = ($)
