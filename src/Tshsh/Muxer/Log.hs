module Tshsh.Muxer.Log
  ( muxLogFile,
    muxLog,
    openMuxLog,
    logSliceList,
  )
where

import Control.Concurrent
import qualified Data.Text as T
import Protolude
import System.IO hiding (hPutStr)
import System.IO.Unsafe
import Tshsh.Data.BufferSlice (SliceList (..))
import qualified Tshsh.Data.BufferSlice as BufferSlice

muxLogFile :: MVar Handle
muxLogFile = unsafePerformIO newEmptyMVar
{-# NOINLINE muxLogFile #-}

muxLog :: Show a => a -> IO ()
muxLog ~a =
  tryReadMVar muxLogFile >>= \case
    Nothing -> pure ()
    Just h -> hPrint h a
{-# INLINE muxLog #-}

openMuxLog :: Text -> IO ()
openMuxLog fName = do
  muxF <- openFile (T.unpack fName) AppendMode
  hSetBuffering muxF LineBuffering
  hSetBinaryMode muxF True
  putMVar muxLogFile muxF

logSliceList :: Text -> Int -> SliceList -> IO ()
logSliceList msg n bl = do
  hPutStr stderr msg
  hPutStr stderr (show $ BufferSlice.listConcat (BufferSlice.listTake n bl) :: Text)
  when (BufferSlice.listLength bl > n) $
    hPutStr stderr ("..." :: Text)
  hPutStr stderr ("\n" :: Text)
