{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Tshsh.Matcher.Seq
  ( mkSeqMatcher,
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
import qualified Data.ByteString as BS
import Protolude
import Tshsh.Stream

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
data SeqMatcher = SeqMatcher
  { _mch_fstChar :: Word8,
    _mch_pos :: {-# UNPACK #-} !Int,
    _mch_maxPos :: {-# UNPACK #-} !Int,
    _mch_pattern :: U.UArray Int Word8,
    _mch_jumpTable :: U.UArray Int Int
  }
  deriving (Eq, Show)

$(makeLenses 'SeqMatcher)

-- Invariants:
-- - mch_pattern is not empty
--
-- - mch_jumpTable indixes are in range [0, T.length mch_pattern]
-- - mch_pos in range [0, T.lengh mch_pattern]
-- - mch_maxPos in range [1, T.length mch_pattern]
mkSeqMatcher_ :: [Word8] -> SeqMatcher
mkSeqMatcher_ [] = panic "SeqMatcher for empty string makes no sense"
mkSeqMatcher_ str@(c : _) =
  let len = length str
   in SeqMatcher
        { _mch_fstChar = c,
          _mch_pattern = U.listArray (0, len -1) str,
          _mch_jumpTable = U.listArray (1, len) (preprocess str),
          _mch_pos = 0,
          _mch_maxPos = len
        }

mkSeqMatcher :: ByteString -> SeqMatcher
mkSeqMatcher str = mkSeqMatcher_ (BS.unpack str)
{-# INLINE mkSeqMatcher #-}

-- Unsafe, can go out of bounds if matcher is ByteString
mch_nextCharUnsafe :: SeqMatcher -> Word8
mch_nextCharUnsafe m =
  fromMaybe (panic "mch_nextCharUnsafe: SeqMatcher is ByteString") $
    m ^? mch_pattern . ix (m ^. mch_pos) -- TODO: check +-1
{-# INLINE mch_nextCharUnsafe #-}

mch_isFull :: SeqMatcher -> Bool
mch_isFull m = m ^. mch_maxPos == m ^. mch_pos
{-# INLINE mch_isFull #-}

-- Unsafe, fails if matcher is ByteString
mch_forwardUnsafe :: SeqMatcher -> SeqMatcher
mch_forwardUnsafe m = m & mch_pos %~ (+ 1)
{-# INLINE mch_forwardUnsafe #-}

matcherReset :: SeqMatcher -> SeqMatcher
matcherReset m = m & mch_pos .~ 0
{-# INLINE matcherReset #-}

data StepResult
  = StepMatch
      { _sr_matchLength :: {-# UNPACK #-} !Int,
        _sr_matcher :: SeqMatcher
      }
  | StepNoMatch {_sr_matcher :: SeqMatcher}
  deriving (Show)

-- Invariants:
-- - accepted matcher should be not ByteString
-- - returned matcher is not ByteString
matcherStep :: SeqMatcher -> Word8 -> StepResult
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
  SeqMatcher ->
  ByteString ->
  Int ->
  ByteString ->
  ConsumerResult SeqMatcher ByteString Int
matchStr_ m0 orig !pos str =
  case BS.uncons str of
    Nothing -> ConsumerContinue m0
    Just (h, t) ->
      case matcherStep m0 h of
        StepMatch _ m' ->
          ConsumerFinish
            { _cs_consumer = m',
              _cs_prev = BS.take (1 + pos) orig,
              _cs_rest = t,
              _cs_result = _mch_maxPos m0
            }
        StepNoMatch m' ->
          if _mch_pos m' == 0
            then -- ByteString.break (== c) compiles into c fast memchr call
            -- which gives c huge speedup in c case when the first letter
            -- of c pattern isn't common in the input string (for example
            -- "\n" or c beginning of an escape sequence in c output from c
            -- shell command

              let c = mch_nextCharUnsafe m'
                  (skip, rest) = BS.break (== c) t
               in matchStr_ m' orig (pos + 1 + BS.length skip) rest
            else matchStr_ m' orig (pos + 1) t
{-# INLINEABLE matchStr_ #-}

matchStr ::
  SeqMatcher ->
  ByteString ->
  ConsumerResult SeqMatcher ByteString Int
matchStr m str = matchStr_ m str 0 str
{-# INLINE matchStr #-}
