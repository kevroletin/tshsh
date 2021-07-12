-- An attempts to interpret program into a smaller subset of operations which
-- doesn't have 'then'. Turned out to be O(n^2)

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

module Lang.Initial.Wrong where

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

data Result i o f a = ResMore (f (i -> f (Result i o f a)))
                    | ResOut o (f (Result i o f a))
                    | ResDone (f a)
                    deriving Functor

chainInterp :: forall i o f a b. Monad f => f (Result i o f a) -> (a -> f (Result i o f b)) -> f (Result i o f b)
chainInterp a f =
  a >>= \case
    ResMore contF -> do
      cont <- contF
      pure . ResMore . pure $ \ev -> chainInterp (cont ev) f
    ResOut out contF -> do
      pure . ResOut out $ chainInterp contF f
    ResDone res -> res >>= f

evalProgram :: forall i o f a . Monad f => Program i o a -> f (Result i o f a)
evalProgram (WaitInput cont) = pure . ResMore . pure $ \inp -> evalProgram (cont inp)
evalProgram (Output out next) = pure . ResOut out $ evalProgram next
evalProgram (Finish a) = pure . ResDone $ pure a
evalProgram (AndThen a b) = chainInterp (evalProgram a) (evalProgram . b)

interp :: Program Text Text a -> IO a
interp (WaitInput cont) = do str <- T.getLine
                             interp (cont str)
interp (Output str next) = do putStrLn str
                              interp next
interp (Finish a) = pure a
interp (AndThen b f) = do b' <- interp b
                          interp (f b')

-----------
-- Tests --
-----------

runBlockingIO :: Program Text Text a -> Int -> IO (Maybe a)
runBlockingIO c n0 = do
  let loop 0 _ = pure Nothing
      loop n (ResMore cont') = do
        cont <- cont'
        str <- T.getLine
        loop (n-1) =<< cont str
      loop n (ResOut str cont) = do
        T.putStrLn str
        loop (n-1) =<< cont
      loop _ (ResDone res') = Just <$> res'
  evalProgram c >>= loop n0

runBlockingIOInt :: Program Int Int a -> IO Int
runBlockingIOInt c  = do
  i :: MVar Int <- newMVar 0
  let loop (ResMore cont') = do
        cont <- cont'
        x <- readMVar i
        putStrLn ("ResMore: " <> show x :: Text)
        loop =<< cont x
      loop (ResOut x cont) = do
        takeMVar i
        putMVar i x
        x <- readMVar i
        putStrLn ("ResOut " <> show x :: Text)
        loop =<< cont
      loop (ResDone res') = do
        x <- readMVar i
        putStrLn ("ResDone" <> show x :: Text)

        res'
  evalProgram c >>= loop

  readMVar i

runBlockingSt :: Program Int Int a -> State Int a
runBlockingSt c  = do
  let loop (ResMore cont') = do
        cont <- cont'
        x <- get
        loop =<< cont x
      loop (ResOut x cont) = do
        put x
        loop =<< cont
      loop (ResDone res') = res'
  evalProgram c >>= loop

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

langLeft n = runState (runBlockingSt (testLeftInt n)) 0
langRight n = runState (runBlockingSt (testRightInt n)) 0

main :: IO ()
-- main = runBlockingIO (testLeftText 50) 100
main = print $ runState (runBlockingSt (testLeftInt 50)) 0
