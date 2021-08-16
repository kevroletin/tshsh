module Tshsh.Data.BufferSlice
  ( BufferSlice (..),
    sliceToByteString,
    sliceFromByteString,
    sliceNull,
    sliceTake,
    sliceTakeEnd,
    sliceDrop,
    sliceDropEnd,
    mergeSlices,
    SliceList (..),
    listAppendEnd,
    listDrop,
    listTake,
    listDropEnd,
    listTakeEnd,
    listConcat,
    listEmpty,
    listLength,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Internal as BS
import Data.String (IsString (..))
import GHC.ForeignPtr
import Protolude hiding (concat)
import Prelude (Show (..))

data BufferSlice = BufferSlice
  { _bs_id :: {-# UNPACK #-} ForeignPtr Word8,
    _bs_slice :: {-# UNPACK #-} ForeignPtr Word8,
    _bs_length :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord)

instance Show BufferSlice where
  show = Prelude.show . sliceToByteString

newtype SliceList = SliceList {unSliceList :: [BufferSlice]} deriving (Eq, Ord, Show)

instance IsString BufferSlice where
  fromString str =
    let bs = C8.pack str
     in sliceFromByteString_ bs bs

sliceFromByteString_ :: ByteString -> ByteString -> BufferSlice
sliceFromByteString_ bsId bsData =
  let (ptr, _, _) = BS.toForeignPtr bsId
   in sliceFromByteString ptr bsData

sliceFromByteString :: ForeignPtr Word8 -> ByteString -> BufferSlice
sliceFromByteString sliceId bs =
  let (ptr, offset, size) = BS.toForeignPtr bs
   in BufferSlice sliceId (plusForeignPtr ptr offset) size
{-# INLINE sliceFromByteString #-}

mergeSlices :: BufferSlice -> BufferSlice -> Maybe BufferSlice
mergeSlices lhs@(BufferSlice id1 buf1 size1) rhs@(BufferSlice id2 buf2 size2)
  | size1 == 0 = Just rhs
  | size2 == 0 = Just lhs
  | id1 == id2 && plusForeignPtr buf1 size1 == buf2 =
    Just (BufferSlice id1 buf1 (size1 + size2))
  | otherwise = Nothing

sliceToByteString :: BufferSlice -> ByteString
sliceToByteString (BufferSlice _ buf size) = BS.fromForeignPtr buf 0 size
{-# INLINE sliceToByteString #-}

sliceNull :: BufferSlice -> Bool
sliceNull (BufferSlice _ _ 0) = True
sliceNull _ = False
{-# INLINE sliceNull #-}

sliceTake :: Int -> BufferSlice -> BufferSlice
sliceTake n (BufferSlice id buf size) = BufferSlice id buf (min size (max 0 n))
{-# INLINE sliceTake #-}

sliceTakeEnd :: Int -> BufferSlice -> BufferSlice
sliceTakeEnd n0 (BufferSlice id buf size) =
  let n = min size (max n0 0)
   in BufferSlice id (plusForeignPtr buf (size - n)) n
{-# INLINE sliceTakeEnd #-}

sliceDrop :: Int -> BufferSlice -> BufferSlice
sliceDrop n0 (BufferSlice id buf size) =
  let n = min size (max n0 0)
   in BufferSlice id (plusForeignPtr buf n) (size - n)
{-# INLINE sliceDrop #-}

sliceDropEnd :: Int -> BufferSlice -> BufferSlice
sliceDropEnd n0 (BufferSlice id buf size) =
  let n = min size (max n0 0)
   in BufferSlice id buf (size - n)
{-# INLINE sliceDropEnd #-}

listAppendEnd :: SliceList -> BufferSlice -> SliceList
listAppendEnd (SliceList []) y = SliceList [y]
listAppendEnd (SliceList (x : xs)) y =
  case mergeSlices x y of
    Just newX -> SliceList (newX : xs)
    Nothing -> SliceList (y : x : xs)
{-# INLINE listAppendEnd #-}

listLength :: SliceList -> Int
listLength = sum . fmap _bs_length . unSliceList
{-# INLINE listLength #-}

listDrop :: Int -> SliceList -> SliceList
listDrop n xs = listTakeEnd (listLength xs - n) xs
{-# INLINE listDrop #-}

listTake :: Int -> SliceList -> SliceList
listTake n xs = listDropEnd (listLength xs - n) xs
{-# INLINE listTake #-}

listDropEnd :: Int -> SliceList -> SliceList
listDropEnd _ (SliceList []) = SliceList []
listDropEnd 0 xs = xs
listDropEnd n (SliceList (bs@(BufferSlice _ _ size) : xs)) =
  if size > n
    then SliceList (sliceDropEnd n bs : xs)
    else listDropEnd (n - size) (SliceList xs)

listTakeEnd :: Int -> SliceList -> SliceList
listTakeEnd n0 (SliceList xs0) = SliceList (go n0 xs0)
  where
    go 0 _ = []
    go _ [] = []
    go n (bs@(BufferSlice _ _ size) : xs) =
      if size > n
        then [sliceTakeEnd n bs]
        else bs : go (n - size) xs

listConcat :: SliceList -> ByteString
listConcat (SliceList []) = BS.empty
listConcat (SliceList xs) = mconcat (sliceToByteString <$> reverse xs)
{-# INLINE listConcat #-}

listEmpty :: SliceList
listEmpty = SliceList []
{-# INLINE listEmpty #-}
