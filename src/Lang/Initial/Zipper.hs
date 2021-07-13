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
{-# LANGUAGE StandaloneDeriving #-}

module Lang.Initial.Zipper where

import Prelude (Show(..))
import Protolude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO (BufferMode(..), hSetBuffering)

data UnionEvidence a where
  IsUnion :: UnionEvidence ()
  NotUnion :: UnionEvidence a

-- i - values we ask
-- o - values we emit
--
--
--
data Program i o s t where
  WaitInput :: (i -> Program i o () t) -> Program i o () t
  Lam :: (s -> Program i o () t) -> Program i o s t
  Output :: o -> Program i o () t -> Program i o () t
  Finish :: t -> Program i o () t
  AndThen :: Program i o s b
             -> Program i o b t
             -> Program i o s t

-- TODO: now executor needs to feed (Just x) and then
-- feed Nothing a few time to get produced outputs
data Res i o r = Cont (Maybe o) (Program i o () r)
               | Res r

instance (Show o) => Show (Program i o a r) where
  show (WaitInput _) = "WaitInput"
  show (Lam _) = "Lam"
  show (Output o _) = "Output " <> Protolude.show o
  show (Finish _) = "Finish "
  show (AndThen a b) = "(AndThen " <> Protolude.show a <> " " <> Protolude.show b <> ")"

deriving instance (Show i, Show o, Show r) => Show (Res i o r)

step :: forall i o a r . Maybe i -> a -> Program i o a r -> Res i o r
step (Just i) _ (WaitInput cont) = step Nothing () (cont i)
step Nothing _ x@(WaitInput _) = Cont Nothing x

step _ a (Lam val) = Cont Nothing (val a)

step Nothing _ (Output x next) = Cont (Just x) next
step (Just _) _ (Output _ _) = panic "Consume all the outputs first"

step _ _ (Finish a) = Res a

step i x (AndThen (AndThen a b) c) = step i x (AndThen a (AndThen b c))
step i a (AndThen val fb) =
  case step i a val of
    --            Nothing mean evaluate without input as fast as we can
    Res b -> step Nothing b fb
    Cont x cont -> Cont x (AndThen cont fb)

loopTest :: Program Int Int () Int
loopTest = let loop = WaitInput (\x -> Output x loop ) in loop

eatAll :: [Int] -> Program Int Int () Int -> IO ()
eatAll [] _ = putStrLn ("End of input" :: Text)
eatAll (x:xs) c0 =
  let loop inp c =
        case step inp () c of
          Cont out cont -> do
            case out of
              Nothing -> eatAll xs cont
              Just x -> do print x
                           loop Nothing cont
          Res x -> putStrLn ("finished with: " <> Protolude.show x :: Text)
  in loop (Just x) c0

stepAllCnt :: Int -> [Int] -> Program Int Int () () -> Int
stepAllCnt res [] _ = res
stepAllCnt !res0 (x:xs) c0 =
  let loop !res inp c =
        case step inp () c of
          Cont out cont -> do
            case out of
              Nothing -> stepAllCnt res xs cont
              Just x -> loop x Nothing cont
          Res x -> res
  in loop res0 (Just x) c0

nop :: Program i o () ()
nop = Finish ()

andThen_ :: Program i o a () -> Program i o () c -> Program i o a c
andThen_ a b = a `AndThen` Lam (const b)

testLeftInt :: Int -> Program Int Int () ()
testLeftInt 0 = nop
testLeftInt n = andThen_ (testLeftInt (n-1))
                         (WaitInput (\n -> Output (n+1) nop))

testRightInt :: Int -> Program Int Int () ()
testRightInt 0 = nop
testRightInt n = andThen_ (WaitInput (\n -> Output (n+1) nop))
                          (testRightInt (n-1))

test_leftCnt :: Int -> Int
test_leftCnt n = stepAllCnt 0 [1..] (testLeftInt n)

test_rightCnt :: Int -> Int
test_rightCnt n = stepAllCnt 0 [1..] (testRightInt n)
