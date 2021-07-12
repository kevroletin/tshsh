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


data Res a = Cont (Program Int Int a)
           | Res a

step :: Int -> Program Int Int a -> Res a
step !i (WaitInput cont) = Cont (cont i)
step !i (Output x next) = -- do somth with x
                         Cont next
step !i (Finish a) = Res a
step !i (AndThen a b) =
  case step i a of
    Res x -> Cont (b x)
    Cont c -> Cont (AndThen c b)

stepAll :: Program Int Int a -> a
stepAll c0 =
  let loop !c = case step 0 c of
                  Cont next -> loop next
                  Res x -> x
  in loop c0

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
