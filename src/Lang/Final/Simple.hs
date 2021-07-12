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

module Lang.Final.Simple where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Protolude
import Prelude ()

-- we are modeling
--
-- data Program i o a = WaitInput (i -> Program i o a)
--                    | Output o (Program i o a)
--                    | Finish a
--                    | forall b . AndThen (Program i o b) (b -> Program i o a)
-- data Res a = Cont (Program Int Int a)
--            | Res a
-- step :: Int -> Program Int Int a -> Res a

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

finalSimpl_main :: IO ()
finalSimpl_main = print $ stepAllCnt 0 [1..] (testRightInt 10000)
