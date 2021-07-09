{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Matcher
  ( mkMatcher,
    step,
    _testMatcher,
  )
where

import Control.Lens
import qualified Data.Array.Unboxed as U
import qualified Data.Text as T
import Protolude
import Test.QuickCheck
import Prelude (String)

takeEnd :: Int -> [a] -> [a]
takeEnd i xs0
  | i <= 0 = []
  | otherwise = f xs0 (drop i xs0)
  where
    f (_ : xs) (_ : ys) = f xs ys
    f xs _ = xs

downTo1 :: (Ord a, Num a) => a -> [a]
downTo1 n = takeWhile (>= 1) $ iterate (\x -> x - 1) n

prefixesDesc :: [a] -> [(Int, [a])]
prefixesDesc str = [(x, take x str) | x <- downTo1 (length str)]

suffixesDesc :: [a] -> [(Int, [a])]
suffixesDesc str = [(x, takeEnd x str) | x <- downTo1 (length str)]

-- TODO: optimize by getting rid of list operations
preprocess :: forall a. Eq a => [a] -> [Int]
preprocess str = reverse $ go (prefixesDesc str)
  where
    go :: [(Int, [a])] -> [Int]
    go [] = []
    go ((_, p) : ps) =
      let suff = drop 1 (suffixesDesc p)
          res = maybe 0 (fst . fst) $ find (uncurry ((==) `on` snd)) $ suff `zip` ps
       in res : go ps

-- all indexes in in Matcher mean "how many characters we've already matched";
-- hence 0 means, we matched nothing and (T.length mch_pattern) means we've
-- matched everything
data Matcher a = Matcher
  { _mch_pattern :: U.UArray Int a,
    _mch_jumpTable :: U.UArray Int Int,
    _mch_pos :: Int,
    _mch_maxPos :: Int
  }

deriving instance Eq (Matcher Char)

deriving instance Show (Matcher Char)

deriving instance Eq (Matcher Word8)

deriving instance Show (Matcher Word8)

$(makeLenses 'Matcher)

type CanUnbox a = U.IArray U.UArray a

-- Invariants:
-- - mch_pattern is not full
--
-- - mch_jumpTable indixes are in range [0, T.length mch_pattern]
-- - mch_pos in range [0, T.lengh mch_pattern]
-- - mch_maxPos in range [1, T.length mch_pattern]
mkMatcher :: forall a. (Eq a, CanUnbox a) => [a] -> Matcher a
mkMatcher str =
  let len = length str
   in if len == 0
        then panic "Matcher for empty string makes no sense"
        else
          Matcher
            { _mch_pattern = U.listArray (0, len -1) str,
              _mch_jumpTable = U.listArray (1, len) (preprocess str),
              _mch_pos = 0,
              _mch_maxPos = len
            }

-- Unsafe, can go out of bounds if matcher is full
mch_nextCharUnsafe :: CanUnbox a => Matcher a -> a
mch_nextCharUnsafe m =
  fromMaybe (panic "mch_nextCharUnsafe: Matcher is full") $
    m ^? mch_pattern . ix (m ^. mch_pos) -- TODO: check +-1

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
    then
      let m = mch_forwardUnsafe m0
       in if mch_isFull m
            then (True, mch_reset m)
            else (False, m)
    else
      if m0 ^. mch_pos == 0
        then (False, m0)
        else
          let Just fallbackPos = m0 ^? mch_jumpTable . ix (m0 ^. mch_pos)
           in step (m0 & mch_pos .~ fallbackPos) inpChr

indices' :: forall a. (CanUnbox a, Eq a) => [a] -> [a] -> [Int]
indices' pat hay = go (mkMatcher pat) ([0 ..] `zip` hay)
  where
    len = length pat

    go :: Matcher a -> [(Int, a)] -> [Int]
    go _ [] = []
    go !m ((i, ch) : xs) =
      case step m ch of
        (True, m') -> 1 + i - len : go m' xs
        (False, m') -> go m' xs

tIndices :: Text -> Text -> [Int]
tIndices a b = T.length . fst <$> T.breakOnAll a b

prop_sameAsTextImpl :: String -> String -> Property
prop_sameAsTextImpl a b =
  not (null a)
    ==> indices' a b == tIndices (T.pack a) (T.pack b)

_testMatcher :: IO ()
_testMatcher = quickCheckWith stdArgs {maxSuccess = 5000} prop_sameAsTextImpl
