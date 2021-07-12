-- While evaluating we turn
--
-- (3)    b->c                   a->b
--       /    \                 /   \
-- (2) a->b    c    into       a    b->c
--     /  \                         / \
-- (1)a    b                       b   c
--
-- This improves complexity of evaluating a program into a coroutine.
-- Interpreter into coroutines evaluates one node at a time and returns the rest
-- of program tree intact. We always evaluate leftmost leave. Evaluating one
-- node rebuilds all the nodes on the path to the root. In the example above if
-- (1) yields then interpreter will rebuild (2) and (3). This leads to O(d^2)
-- complexity where d is depth of an evaluated node.
--
-- Doing transformation above can be viewed as using a zipper to focus on the
-- currently evaluated node.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Lang.Initial.Zipper where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Protolude
import Prelude ()

data ProgramG i o a where
  WaitInput :: (i -> ProgramG i o a) -> ProgramG i o a
  Output :: o -> ProgramG i o a -> ProgramG i o a
  Finish :: a -> ProgramG i o a
  AndThen :: ProgramG i o b -> (b -> ProgramG i o a) -> ProgramG i o a

data Res i o a
  = Cont (Maybe o) (R i o a)
  | Res a

newtype R i o a = R {step :: Maybe i -> Res i o a}

waitInput :: (i -> R i o a) -> R i o a
waitInput cont = R $ \case
  Nothing -> Cont Nothing (waitInput cont)
  Just x -> step (cont x) Nothing

output :: o -> R i o a -> R i o a
output o next = R $ \case
  Nothing -> Cont (Just o) next
  Just x ->
    panic "Eat all output first"
    -- or just ignore output
    step next (Just x)

finish :: a -> R i o a
finish a = R . const $ Res a

andThen :: R i o b -> (b -> R i o a) -> R i o a
andThen a b =
  R $ \i ->
    case step a i of
      Cont o cont -> Cont o (andThen cont b)
      Res x -> Cont Nothing (b x)

test1 :: R Int Int Int
test1 =       waitInput      $
        \i -> output (i + 1) $
              finish (i + 2)

--- Tests

test = test1 `andThen` const test1

stepAllIO :: [Int] -> R Int Int Int -> IO ()
stepAllIO [] _ = putStrLn ("End of input" :: Text)
stepAllIO (x0:xs) r0 =
  let loop x r = case step r x of
        Cont Nothing cont -> stepAllIO xs cont
        Cont (Just o) cont -> do putStrLn ("Output: " <> show o :: Text)
                                 stepAllIO xs cont
        Res x -> putStrLn ("Result: " <> show x :: Text)
  in loop (Just x0) r0

andThen_ a b = a `andThen` const b
nop = finish ()

testLeftInt :: Int -> R Int Int ()
testLeftInt 0 = nop
testLeftInt n = andThen_ (testLeftInt (n-1))
                         (waitInput (\n -> output (n+1) nop))

testRightInt :: Int -> R Int Int ()
testRightInt 0 = nop
testRightInt n = andThen_ (waitInput (\n -> output (n+1) nop))
                          (testRightInt (n-1))


stepAllCnt :: Int -> [Int] -> R Int Int () -> Int
stepAllCnt !res [] _ = res
stepAllCnt !res (x0:xs) r0 =
  let loop x r = case step r x of
        Cont Nothing cont -> stepAllCnt res xs cont
        Cont (Just o) cont -> stepAllCnt o xs cont
        Res () -> res
  in loop (Just x0) r0

main :: IO ()
main = print $ stepAllCnt 0 [1..] (testLeftInt 10000)
