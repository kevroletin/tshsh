{-# LANGUAGE ConstraintKinds, FlexibleContexts, ScopedTypeVariables, StandaloneDeriving, ViewPatterns, RecordWildCards, OverloadedStrings, BangPatterns, TemplateHaskell #-}

module MatcherParam where

import Prelude (String)
import Protolude

import qualified Data.Text as T
import qualified Data.Text.Internal.Search as T
import Control.Lens
import Data.List.Extra

import qualified Data.Array.Unboxed as U

import Test.QuickCheck

downTo1 :: (Ord a, Num a) => a -> [a]
downTo1 n = takeWhile (>=1) $ iterate (\x -> x - 1) n

prefixesDesc :: [a] -> [(Int, [a])]
prefixesDesc str = [(x, take x str) | x <- downTo1 (length str)]

suffixesDesc :: [a] -> [(Int, [a])]
suffixesDesc str = [(x, takeEnd x str) | x <- downTo1 (length str)]

preprocess :: forall a . Eq a => [a] -> [Int]
preprocess str = reverse $ go (prefixesDesc str)
  where
    go :: [(Int, [a])] -> [Int]
    go [] = []
    go !(!(_, p) : ps) =
      let suff = drop 1 (suffixesDesc p)
          res = maybe 0 (fst . fst) $ find (uncurry ((==) `on` snd)) $ suff `zip` ps
      in res : go ps

-- all indexes in in Matcher mean "how many characters we've already matched";
-- hence 0 means, we matched nothing and (T.length mch_pattern) means we've
-- matched everything
data Matcher a = Matcher { _mch_pattern :: U.UArray Int a,
                           _mch_jumpTable :: U.UArray Int Int,
                           _mch_pos :: Int,
                           _mch_maxPos :: Int
                         }

-- deriving instance (U.IArray U.UArray a, Show a) => Show (Matcher a)
-- deriving instance Eq a => Eq (Matcher a)

$(makeLenses 'Matcher)

type CanUnbox a = U.IArray U.UArray a

-- Invariants:
-- - mch_pattern is not full
--
-- - mch_jumpTable indixes are in range [0, T.length mch_pattern]
-- - mch_pos in range [0, T.lengh mch_pattern]
-- - mch_maxPos in range [1, T.length mch_pattern]
mkMatcher :: forall a . (Eq a, CanUnbox a) => [a] -> Matcher a
mkMatcher str =
  let len = length str in
  if len == 0
    then panic "Matcher for empty string makes no sense"
    else Matcher { _mch_pattern = U.listArray (0, len-1) str,
                   _mch_jumpTable = U.listArray (1, len) (preprocess str),
                   _mch_pos = 0,
                   _mch_maxPos = len
                }

-- Unsafe, can go out of bounds if matcher is full
mch_nextCharUnsafe :: CanUnbox a => Matcher a -> a
mch_nextCharUnsafe m = fromMaybe (panic "oops") $ m ^? mch_pattern . ix (m ^. mch_pos) -- TODO: check +-1

mch_isFull :: Matcher a -> Bool
mch_isFull m = m ^. mch_maxPos == m ^. mch_pos

-- Unsafe, fails if matcher is full
mch_forwardUnsafe :: Matcher a -> Matcher a
mch_forwardUnsafe m = m & mch_pos %~ (\x -> x + 1)

mch_reset :: Matcher a -> Matcher a
mch_reset m = m & mch_pos .~ 0

-- Invariants:
-- - accepted matcher should be not full
-- - returned matcher is not full
step :: (Eq a, CanUnbox a) => Matcher a -> a -> (Bool, Matcher a)
step !m0 !inpChr =
  if mch_nextCharUnsafe m0 == inpChr
    then let m = mch_forwardUnsafe m0
          in if mch_isFull m
                then (True,  mch_reset m)
                else (False, m)
    else
      if m0 ^. mch_pos == 0
        then (False, m0)
        else let Just fallbackPos = m0 ^? mch_jumpTable . ix (m0 ^. mch_pos)
              in step (m0 & mch_pos .~ fallbackPos) inpChr

indices' :: forall a . (CanUnbox a, Eq a) => [a] -> [a] -> [Int]
indices' pat hay = go (mkMatcher pat) ([0..] `zip` hay)
  where
    len = length pat

    go :: Matcher a -> [(Int, a)] -> [Int]
    go _ [] = []
    go !m !((i, ch) : xs) =
      case step m ch of
        (True, m') -> 1 + i - len : go m' xs
        (False, m') -> go m' xs


run :: (Eq a, CanUnbox a) => [a] -> [a] -> [(a, Bool, Matcher a)]
run pat inp =
  let go !m (c : cs) = let (b, m') = step m c
                       in (c, b, m') : go m' cs
      go _ [] = []
  in go (mkMatcher pat) inp

prop_correct :: String -> String -> Property
prop_correct a b =
  not (null a) ==>
    indices' a b == T.indices (T.pack a) (T.pack b)

main :: IO ()
-- main = quickCheck prop_correct
main = quickCheckWith stdArgs { maxSuccess = 5000 } prop_correct

-- " "
-- "\1028440 "
