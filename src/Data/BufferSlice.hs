module Data.BufferSlice
  ( BufferSlice (..),
    sliceToByteString,
    sliceFromByteString,
    mergeSlices,
    SliceList (..),
    listAppendEnd,
    listDropEnd,
    listConcat,
    listEmpty,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import GHC.ForeignPtr
import Protolude hiding (concat, listEmpty)
import Prelude (Show (..))

data BufferSlice = BufferSlice
  { _bs_id :: {-# UNPACK #-} ForeignPtr Word8,
    _bs_slice :: {-# UNPACK #-} ForeignPtr Word8,
    _bs_length :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord)

instance Show BufferSlice where
  show = Prelude.show . sliceToByteString

newtype SliceList = SliceList [BufferSlice] deriving (Eq, Ord, Show)

sliceFromByteString :: ForeignPtr Word8 -> ByteString -> BufferSlice
sliceFromByteString sliceId bs =
  let (ptr, offset, size) = BS.toForeignPtr bs
   in BufferSlice sliceId (plusForeignPtr ptr offset) size

mergeSlices :: BufferSlice -> BufferSlice -> Maybe BufferSlice
mergeSlices lhs@(BufferSlice id1 buf1 size1) rhs@(BufferSlice id2 buf2 size2)
  | size1 == 0 = Just rhs
  | size2 == 0 = Just lhs
  | id1 == id2 && plusForeignPtr buf1 size1 == buf2 =
    Just (BufferSlice id1 buf1 (size1 + size2))
  | otherwise = Nothing

sliceToByteString :: BufferSlice -> ByteString
sliceToByteString (BufferSlice _ buf size) = BS.fromForeignPtr buf 0 size

listAppendEnd :: SliceList -> BufferSlice -> SliceList
listAppendEnd (SliceList []) y = SliceList [y]
listAppendEnd (SliceList (x : xs)) y =
  case mergeSlices x y of
    Just newX -> SliceList (newX : xs)
    Nothing -> SliceList (y : x : xs)

listDropEnd :: Int -> SliceList -> SliceList
listDropEnd _ (SliceList []) = SliceList []
listDropEnd 0 xs = xs
listDropEnd n (SliceList (BufferSlice id buf size : xs)) =
  if size > n
    then SliceList (BufferSlice id buf (size - n) : xs)
    else listDropEnd (n - size) (SliceList xs)

listConcat :: SliceList -> ByteString
listConcat (SliceList []) = BS.empty
listConcat (SliceList xs) = mconcat (sliceToByteString <$> reverse xs)

listEmpty :: SliceList
listEmpty = SliceList []
