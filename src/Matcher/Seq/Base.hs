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

module Matcher.Seq.Base
  ( mkMatcher,
    matcherStep,
    matcherReset,
    matchStr,
    SeqMatcher (..),
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
import qualified Matcher.Result as R
import Protolude

takeEnd :: Int -> [c] -> [c]
takeEnd i xs0
  | i <= 0 = []
  | otherwise = f xs0 (drop i xs0)
  where
    f (_ : xs) (_ : ys) = f xs ys
    f xs _ = xs

downTo1 :: (Ord c, Num c) => c -> [c]
downTo1 n = takeWhile (>= 1) $ iterate (\x -> x - 1) n

prefixesDesc :: [c] -> [(Int, [c])]
prefixesDesc str = [(x, take x str) | x <- downTo1 (length str)]

suffixesDesc :: [c] -> [(Int, [c])]
suffixesDesc str = [(x, takeEnd x str) | x <- downTo1 (length str)]

-- TODO: optimize by getting rid of list operations
preprocess :: forall c. Eq c => [c] -> [Int]
preprocess str = reverse $ go (prefixesDesc str)
  where
    go :: [(Int, [c])] -> [Int]
    go [] = []
    go ((_, p) : ps) =
      let suff = drop 1 (suffixesDesc p)
          res = maybe 0 (fst . fst) $ find (uncurry ((==) `on` snd)) $ suff `zip` ps
       in res : go ps

-- all indexes in in SeqMatcher mean "how many characters we've already matched";
-- hence 0 means, we matched nothing and (T.length mch_pattern) means we've
-- matched everything
data SeqMatcher c a = SeqMatcher
  { _mch_fstChar :: !c,
    _mch_pos :: {-# UNPACK #-} !Int,
    _mch_maxPos :: {-# UNPACK #-} !Int,
    _mch_pattern :: U.UArray Int c,
    _mch_jumpTable :: U.UArray Int Int,
    _mch_ret :: a
  }

deriving instance Eq a => Eq (SeqMatcher Char a)

deriving instance Show a => Show (SeqMatcher Char a)

deriving instance Eq a => Eq (SeqMatcher Word8 a)

deriving instance Show a => Show (SeqMatcher Word8 a)

$(makeLenses 'SeqMatcher)

type CanUnbox c = U.IArray U.UArray c

-- Invariants:
-- - mch_pattern is not full
--
-- - mch_jumpTable indixes are in range [0, T.length mch_pattern]
-- - mch_pos in range [0, T.lengh mch_pattern]
-- - mch_maxPos in range [1, T.length mch_pattern]
mkMatcher' :: (Eq c, CanUnbox c) => a -> [c] -> SeqMatcher c a
mkMatcher' _ [] = panic "SeqMatcher for empty string makes no sense"
mkMatcher' a str@(c : _) =
  let len = length str
   in SeqMatcher
        { _mch_fstChar = c,
          _mch_pattern = U.listArray (0, len -1) str,
          _mch_jumpTable = U.listArray (1, len) (preprocess str),
          _mch_pos = 0,
          _mch_maxPos = len,
          _mch_ret = a
        }

mkMatcher :: (Eq item, CanUnbox item, ListLike full item) => a -> full -> SeqMatcher item a
mkMatcher a str = mkMatcher' a (L.toList str)
{-# INLINE mkMatcher #-}

-- Unsafe, can go out of bounds if matcher is full
mch_nextCharUnsafe :: CanUnbox c => SeqMatcher c a -> c
mch_nextCharUnsafe m =
  fromMaybe (panic "mch_nextCharUnsafe: SeqMatcher is full") $
    m ^? mch_pattern . ix (m ^. mch_pos) -- TODO: check +-1
{-# INLINE mch_nextCharUnsafe #-}

mch_isFull :: SeqMatcher c a -> Bool
mch_isFull m = m ^. mch_maxPos == m ^. mch_pos
{-# INLINE mch_isFull #-}

-- Unsafe, fails if matcher is full
mch_forwardUnsafe :: SeqMatcher c a -> SeqMatcher c a
mch_forwardUnsafe m = m & mch_pos %~ (+ 1)
{-# INLINE mch_forwardUnsafe #-}

matcherReset :: SeqMatcher c a -> SeqMatcher c a
matcherReset m = m & mch_pos .~ 0
{-# INLINE matcherReset #-}

-- Invariants:
-- - accepted matcher should be not full
-- - returned matcher is not full
matcherStep :: (Eq c, CanUnbox c) => SeqMatcher c a -> c -> R.StepResult (SeqMatcher c a) a
matcherStep m !inpChr
  | _mch_pos m == 0 && (inpChr /= _mch_fstChar m) = R.StepNoMatch m
  | _mch_pos m == 0 || mch_nextCharUnsafe m == inpChr =
    let m' = mch_forwardUnsafe m
     in if mch_isFull m'
          then R.StepMatch (_mch_maxPos m') (matcherReset m') (_mch_ret m')
          else R.StepNoMatch m'
  | otherwise =
    let fallbackPos = _mch_jumpTable m ! _mch_pos m
     in matcherStep (m {_mch_pos = fallbackPos}) inpChr
{-# INLINEABLE matcherStep #-}

matchStr_ ::
  (Eq item, CanUnbox item, ListLike full item) =>
  SeqMatcher item a ->
  full ->
  Int ->
  full ->
  R.MatchResult (SeqMatcher item a) full a
matchStr_ m0 orig !pos str =
  case L.uncons str of
    Nothing -> R.NoMatch m0
    Just (h, t) ->
      case matcherStep m0 h of
        R.StepMatch _ m' ret -> R.Match m' (_mch_maxPos m0) (L.take (1 + pos) orig) t ret
        R.StepNoMatch m' ->
          if _mch_pos m' == 0
            then -- ByteString.break (== c) compiles into c fast memchr call
                 -- which gives c huge speedup in c case when the first letter
                 -- of c pattern isn't common in the input string (for example
                 -- "\n" or c beginning of an escape sequence in c output from c
                 -- shell command
              let c = mch_nextCharUnsafe m'
                  (skip, rest) = L.break (== c) t
               in matchStr_ m' orig (pos + 1 + L.length skip) rest
            else matchStr_ m' orig (pos + 1) t
{-# INLINEABLE matchStr_ #-}

matchStr ::
  (Eq item, CanUnbox item, ListLike full item) =>
  SeqMatcher item a ->
  full ->
  R.MatchResult (SeqMatcher item a) full a
matchStr m str = matchStr_ m str 0 str
{-# INLINEABLE matchStr #-}
