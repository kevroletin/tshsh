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
    Matcher,
    mch_pattern,
    mch_jumpTable,
    mch_pos,
    mch_maxPos,
  )
where

import Control.Lens
import qualified Data.Array.Unboxed as U
import Protolude
import Matcher.Result

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
    _mch_pos :: {-# UNPACK #-} !Int,
    _mch_maxPos :: {-# UNPACK #-} !Int
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

matcherReset :: Matcher a -> Matcher a
matcherReset m = m & mch_pos .~ 0

-- Invariants:
-- - accepted matcher should be not full
-- - returned matcher is not full

matcherStep :: (Eq a, CanUnbox a) => Matcher a -> a -> StepResult (Matcher a)
matcherStep !m0 !inpChr =
  if mch_nextCharUnsafe m0 == inpChr
    then
      let m = mch_forwardUnsafe m0
       in if mch_isFull m
            then StepMatch (m ^. mch_maxPos) (matcherReset m)
            else StepNoMatch m
    else
      if m0 ^. mch_pos == 0
        then StepNoMatch m0
        else
          let Just fallbackPos = m0 ^? mch_jumpTable . ix (m0 ^. mch_pos)
           in matcherStep (m0 & mch_pos .~ fallbackPos) inpChr

-- data BuffStepResult a
--   = BuffStepMatch [a] (Matcher a)
--   | BuffStepNoMatch [a] (Matcher a)

-- $(makePrisms 'BuffStepNoMatch)

-- deriving instance Eq (BuffStepResult Char)

-- deriving instance Show (BuffStepResult Char)

-- deriving instance Eq (BuffStepResult Word8)

-- deriving instance Show (BuffStepResult Word8)

-- bufferedMatcherStep_ :: (Eq a, CanUnbox a) => Int -> Matcher a -> a -> BuffStepResult a
-- bufferedMatcherStep_ skipped !m0 !inpChr =
--   if mch_nextCharUnsafe m0 == inpChr
--     then
--       let m = mch_forwardUnsafe m0
--        in if mch_isFull m
--             then BuffStepMatch (U.elems (m0 ^. mch_pattern)) (matcherReset m)
--             else BuffStepNoMatch [] m
--     else
--       if m0 ^. mch_pos == 0
--         then BuffStepNoMatch ((take skipped . U.elems $ m0 ^. mch_pattern) ++ [inpChr]) m0
--         else
--           let Just fallbackPos = m0 ^? mch_jumpTable . ix (m0 ^. mch_pos)
--               dPos = (m0 ^. mch_pos) - fallbackPos
--            in bufferedMatcherStep_ (skipped + dPos) (m0 & mch_pos .~ fallbackPos) inpChr

-- -- output characters as long as we understand that it's not part of a match
-- bufferedMatcherStep :: (Eq a, CanUnbox a) => Matcher a -> a -> BuffStepResult a
-- bufferedMatcherStep = bufferedMatcherStep_ 0
