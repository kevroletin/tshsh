module Tshsh.Muxer.Log
  ( muxLogFile,
    muxLog,
    openMuxLog,
  )
where

import Control.Concurrent
import qualified Data.Text as T
import Protolude
import System.IO
import System.IO.Unsafe

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
