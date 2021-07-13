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

module Lang.Final.NoData where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Protolude
import Prelude ()

-- data Res i o t
--   = Cont (Maybe o) (R i o () t)
--   | Res t

type ResC i o t = forall r . (Maybe o -> R i o () t -> r)
                          -> (t -> r)
                          -> r

contC :: Maybe o -> R i o () t -> ResC i o t
contC a b = \c _ -> c a b

resC :: t -> ResC i o t
resC a = \_ c -> c a

showRes :: (Show o, Show t) => ResC i o t -> Text
showRes r =
  r (\o _ -> "Cont " <> show o <> " _")
    (\t -> "Res " <> show t)

type OptC i o s t = forall r . (forall w . R i o s w -> R i o w t -> r)
                            -> r
                            -> r

optC :: R i o s w -> R i o w t -> OptC i o s t
optC a b = \c _ -> c a b

noOptC :: OptC i o s t
noOptC _ c = c

data Opt i o s t = forall w . Opt (R i o s w) (R i o w t)
                 | NoOpt

-- data R i o s t = R { optimize :: OptC i o s t,
--                      step :: s -> Maybe i -> ResC i o t
--                    }

newtype R i o s t = R { runR :: forall r . ( OptC i o s t -> (s -> Maybe i -> ResC i o t) -> r) -> r }

rC :: OptC i o s t -> (s -> Maybe i -> ResC i o t) -> R i o s t
rC a b = R $ \c -> c a b

stepC :: R i o s t -> ((s -> Maybe i -> ResC i o t) -> r) -> r
stepC r c = runR r $ \_ step -> c step

step :: R i o s t -> s -> Maybe i -> ResC i o t
step r a b = stepC r $ \step -> step a b

optimizeC :: R i o s t -> (OptC i o s t -> r) -> r
optimizeC r c = runR r $ \optimize _ -> c optimize

optimize :: R i o s t -> (forall w . R i o s w -> R i o w t -> r) -> r -> r
optimize r a b = optimizeC r $ \optimize -> optimize a b

waitInput :: (i -> R i o () t) -> R i o () t
waitInput cont = rC noOptC $ \() i ->
  case i of
    Nothing -> contC Nothing (waitInput cont)
    Just x -> step (cont x) () Nothing

output :: o -> R i o () a -> R i o () a
output o next = rC noOptC $ \() i ->
  case i of
    Nothing -> contC (Just o) next
    Just x ->
      -- panic "Eat all output first"
      -- or just ignore output
      step next () (Just x)

lam :: (s -> R i o () r) -> R i o s r
lam cont = rC noOptC $ \s i -> contC Nothing (cont s)

finish :: a -> R i o () a
finish a = rC noOptC $ \() _ -> resC a

-- TODO: explore if we can combine inlining and rewrite rules to optimize programs.
andThen :: R i o s w -> R i o w t -> R i o s t
andThen a b =
  rC (optC a b) $ \s i ->
    optimize a
      (\a' c ->
        step (andThen a' (andThen c b)) s Nothing)
      (step a s i
        (\o cont -> contC o (andThen cont b))
        (\x -> stepC b $ \step -> step x Nothing))

test1 :: R Int Int () Int
test1 =       waitInput      $
        \i -> output (i + 1) $
              output (i + 1) $
              finish (i + 2)

-- --- Tests

stepAllIO :: [Int] -> R Int Int () Int -> IO ()
stepAllIO [] _ = putStrLn ("End of input" :: Text)
stepAllIO (x0:xs) r0 =
  let loop x r = step r () x
        (\i cont ->
           case i of
              Nothing -> stepAllIO xs cont
              (Just o) -> do putStrLn ("Output: " <> show o :: Text)
                             loop Nothing cont)
        (\x -> putStrLn ("Result: " <> show x :: Text))
  in loop (Just x0) r0

andThen_ a b = a `andThen` lam (const b)
nop = finish ()

testLeftInt :: Int -> R Int Int () ()
testLeftInt 0 = nop
testLeftInt n = andThen_ (testLeftInt (n-1))
                         (waitInput (\n -> output (n+1) nop))

testRightInt :: Int -> R Int Int () ()
testRightInt 0 = nop
testRightInt n = andThen_ (waitInput (\n -> output (n+1) nop))
                          (testRightInt (n-1))

stepAllCnt :: Int -> [Int] -> R Int Int () () -> Int
stepAllCnt !res [] _ = res
stepAllCnt !res0 (x0:xs) r0 =
  let loop !res !r !x = step r () x
        (\i cont ->
           case i of
              Nothing -> stepAllCnt res xs cont
              (Just o) -> loop o cont Nothing)
        (\() -> res)
  in loop res0 r0 (Just x0)

test_leftCnt :: Int -> Int
test_leftCnt n = stepAllCnt 0 [1..] (testLeftInt n)

test_rightCnt :: Int -> Int
test_rightCnt n = stepAllCnt 0 [1..] (testRightInt n)

printLoop :: R Int Int () Int
printLoop = waitInput $ \i -> output i printLoop

testLoop :: Int -> IO ()
testLoop n = stepAllIO [1..n] printLoop

main :: IO ()
main = testLoop 10
