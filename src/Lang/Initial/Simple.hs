-- Interpreting Program into IO directly in linear time. Interpreting into a
-- coroutine poses a potential for O(n^2) execution time in case of imbalanced
-- computation tree.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}

module Lang.Initial.Simple where

import Prelude ()
import Protolude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO (BufferMode(..), hSetBuffering)


data Program i o a = WaitInput (i -> Program i o a)
                   | Output o (Program i o a)
                   | Finish a
                   | forall b . AndThen (Program i o b) (b -> Program i o a)

interp' :: MVar Int -> Program Int Int a -> IO a
interp' i (WaitInput cont) = do x <- readMVar i
                                interp' i (cont x)
interp' i (Output x next) = do takeMVar i
                               putMVar i x
                               interp' i next
interp' i (Finish a) = pure a
interp' i (AndThen b f) = do b' <- interp' i b
                             interp' i (f b')


data Res i o s = Cont (Maybe o) (Program i o s)
               | Res s

step :: Maybe i -> Program i o s -> Res i o s
step (Just i) (WaitInput cont) = step Nothing (cont i)
step Nothing (WaitInput cont) = Cont Nothing (WaitInput cont)
step (Just _) (Output x next) = panic "You should consume all output first"
step Nothing (Output x next) = Cont (Just x) next
step (Just _) (Finish a) = panic "You should consume returned value"
step Nothing (Finish a) = Res a
step i (AndThen a b) =
  case step i a of
    Res x -> step Nothing (b x)
    Cont o c -> Cont o (AndThen c b)

stepAllCnt :: forall a. Int -> [Int] -> Program Int Int () -> Int
stepAllCnt res [] _ = res
stepAllCnt res0 (x:xs) c0 =
  let loop !res c =
        case c of
          Cont (Just o) next -> loop o (step Nothing next)
          Cont Nothing next -> stepAllCnt res xs next
          Res () -> res
  in loop res0 (step (Just x) c0)

-----------
-- Tests --
-----------

andThen_ a b = a `AndThen` const b
nop = Finish ()

testLeftText :: Int -> Program Text Text ()
testLeftText 0 = nop
testLeftText n = andThen_ (testLeftText (n-1))
                          (Output (show n) nop)

testRightText :: Int -> Program Text Text ()
testRightText 0 = nop
testRightText n = andThen_ (Output (show n) nop)
                           (testRightText (n-1))
testLeftInt :: Int -> Program Int Int ()
testLeftInt 0 = nop
testLeftInt n = andThen_ (testLeftInt (n-1))
                         (WaitInput (\n -> Output (n+1) nop))

testRightInt :: Int -> Program Int Int ()
testRightInt 0 = nop
testRightInt n = andThen_ (WaitInput (\n -> Output (n+1) nop))
                          (testRightInt (n-1))

test_leftCnt :: Int -> Int
test_leftCnt n = stepAllCnt 0 [1..] (testLeftInt n)

test_rightCnt :: Int -> Int
test_rightCnt n = stepAllCnt 0 [1..] (testRightInt n)
