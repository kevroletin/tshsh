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

module Matcher.Seq.Unboxed
  ( mkMatcher,
    matcherStep,
    matcherReset,
    matchStr,
    Matcher (..),
    mch_fstChar,
    mch_pattern,
    mch_jumpTable,
    mch_pos,
    mch_maxPos,
    mch_nextCharUnsafe,
    mch_isFull,
    mch_forwardUnsafe,
  )
where

import Control.Lens
import Data.Array.IArray
import qualified Data.Array.Unboxed as U
import Data.ListLike (ListLike)
import qualified Data.ListLike as L
import Matcher.Result
import qualified Matcher.Result as R
import Protolude

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
  { _mch_fstChar :: !a,
    _mch_pos :: {-# UNPACK #-} !Int,
    _mch_maxPos :: {-# UNPACK #-} !Int,
    _mch_pattern :: U.UArray Int a,
    _mch_jumpTable :: U.UArray Int Int
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
mkMatcher' :: forall a. (Eq a, CanUnbox a) => [a] -> Matcher a
mkMatcher' [] = panic "Matcher for empty string makes no sense"
mkMatcher' str@(a : _) =
  let len = length str
   in Matcher
        { _mch_fstChar = a,
          _mch_pattern = U.listArray (0, len -1) str,
          _mch_jumpTable = U.listArray (1, len) (preprocess str),
          _mch_pos = 0,
          _mch_maxPos = len
        }

mkMatcher :: (Eq item, CanUnbox item, ListLike full item) => full -> Matcher item
mkMatcher str = mkMatcher' (L.toList str)
{-# INLINE mkMatcher #-}

-- Unsafe, can go out of bounds if matcher is full
mch_nextCharUnsafe :: CanUnbox a => Matcher a -> a
mch_nextCharUnsafe m =
  fromMaybe (panic "mch_nextCharUnsafe: Matcher is full") $
    m ^? mch_pattern . ix (m ^. mch_pos) -- TODO: check +-1
{-# INLINE mch_nextCharUnsafe #-}

mch_isFull :: Matcher a -> Bool
mch_isFull m = m ^. mch_maxPos == m ^. mch_pos
{-# INLINE mch_isFull #-}

-- Unsafe, fails if matcher is full
mch_forwardUnsafe :: Matcher a -> Matcher a
mch_forwardUnsafe m = m & mch_pos %~ (+ 1)
{-# INLINE mch_forwardUnsafe #-}

matcherReset :: Matcher a -> Matcher a
matcherReset m = m & mch_pos .~ 0
{-# INLINE matcherReset #-}

-- Invariants:
-- - accepted matcher should be not full
-- - returned matcher is not full
matcherStep :: (Eq a, CanUnbox a) => Matcher a -> a -> StepResult (Matcher a)
matcherStep m !inpChr
  | _mch_pos m == 0 && (inpChr /= _mch_fstChar m) = StepNoMatch m
  | _mch_pos m == 0 || mch_nextCharUnsafe m == inpChr =
    let m' = mch_forwardUnsafe m
     in if mch_isFull m'
          then StepMatch (_mch_maxPos m') (matcherReset m')
          else StepNoMatch m'
  | otherwise =
    let fallbackPos = _mch_jumpTable m ! _mch_pos m
     in matcherStep (m {_mch_pos = fallbackPos}) inpChr
{-# INLINEABLE matcherStep #-}

matchStr_ ::
  (Eq item, CanUnbox item, ListLike full item) =>
  Matcher item ->
  full ->
  Int ->
  full ->
  R.MatchResult (Matcher item) full
matchStr_ m0 orig !pos str =
  case L.uncons str of
    Nothing -> R.NoMatch m0
    Just (h, t) ->
      case matcherStep m0 h of
        R.StepMatch _ m' -> R.Match m' (_mch_maxPos m0) (L.take (1 + pos) orig) t
        R.StepNoMatch m' ->
          if _mch_pos m' == 0
            then
              let c = mch_nextCharUnsafe m'
                  (skip, rest) = L.break (== c) t
               in matchStr_ m' orig (pos + 1 + L.length skip) rest
            else matchStr_ m' orig (pos + 1) t
{-# INLINEABLE matchStr_ #-}

matchStr ::
  (Eq item, CanUnbox item, ListLike full item) =>
  Matcher item ->
  full ->
  MatchResult (Matcher item) full
matchStr m str = matchStr_ m str 0 str
{-# INLINEABLE matchStr #-}
