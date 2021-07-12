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

module Lang.Final.Zipper where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Protolude
import Prelude ()

data Res i o t
  = Cont (Maybe o) (R i o () t)
  | Res t

showRes :: (Show o, Show t) => Res i o t -> Text
showRes (Cont o _ ) = "Cont " <> show o <> " _"
showRes (Res t) = "Res " <> show t

data Opt i o s t = forall w . Opt (R i o s w) (R i o w t)
                 | NoOpt

data R i o s t = R { optimize :: Opt i o s t,
                     step :: s -> Maybe i -> Res i o t
                   }

waitInput :: (i -> R i o () t) -> R i o () t
waitInput cont = R NoOpt $ \() i ->
  case i of
    Nothing -> Cont Nothing (waitInput cont)
    Just x -> step (cont x) () Nothing

output :: o -> R i o () a -> R i o () a
output o next = R NoOpt $ \() i ->
  case i of
    Nothing -> Cont (Just o) next
    Just x ->
      panic "Eat all output first"
      -- or just ignore output
      step next () (Just x)

lam :: (s -> R i o () r) -> R i o s r
lam cont = R NoOpt $ \s i -> Cont Nothing (cont s)

finish :: a -> R i o () a
finish a = R NoOpt $ \() _ -> Res a

andThen :: R i o s w -> R i o w t -> R i o s t
andThen a b =
  R (Opt a b) $ \s i ->
    case optimize a of
      NoOpt ->
        case step a s i of
          Cont o cont -> Cont o (andThen cont b)
          Res x -> step b x Nothing
      Opt a' c ->
        step (andThen a' (andThen c b)) s Nothing

test1 :: R Int Int () Int
test1 =       waitInput      $
        \i -> output (i + 1) $
              output (i + 1) $
              finish (i + 2)

--- Tests

test = test1 `andThen` lam (const test1)

stepAllIO :: [Int] -> R Int Int () Int -> IO ()
stepAllIO [] _ = putStrLn ("End of input" :: Text)
stepAllIO (x0:xs) r0 =
  let loop x r = case step r () x of
        Cont Nothing cont -> stepAllIO xs cont
        Cont (Just o) cont -> do putStrLn ("Output: " <> show o :: Text)
                                 loop Nothing cont
        Res x -> putStrLn ("Result: " <> show x :: Text)
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
  let loop !res r x = case step r () x of
        Cont Nothing cont -> stepAllCnt res xs cont
        Cont (Just o) cont -> loop o cont Nothing
        Res () -> res
  in loop res0 r0 (Just x0)

main :: IO ()
main = print $ stepAllCnt 0 [1..] (testRightInt 1000)
-- main = stepAllIO [1..] test
-- main = do let (Cont Nothing c) = step test1 () (Just 1)
--           print . showRes $ step c () (Just 2)
--           pure ()
