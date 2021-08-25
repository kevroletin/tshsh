module Tshsh.ReadLoop
  ( readLoopStep,
    readLoopInit,
    readLoop,
    ReadLoopSt,
  )
where

import Control.Exception.Safe (tryIO)
import Control.Monad
import Foreign hiding (void)
import Protolude hiding (log, tryIO)
import System.IO (hGetBufSome)
import qualified Tshsh.Constants as Const
import Tshsh.Data.BufferSlice (BufferSlice (..))

data ReadLoopSt = ReadLoopSt
  { _rl_capacity :: Int,
    _rl_buff :: ForeignPtr Word8,
    _rl_dataPtr :: ForeignPtr Word8,
    _rl_fileHandle :: Handle
  }

readLoopStep :: ReadLoopSt -> IO (Maybe (BufferSlice, ReadLoopSt))
readLoopStep st0 =
  if _rl_capacity st0 >= Const.minBufSize
    then readSlice st0
    else do
      buff <- mallocForeignPtrBytes Const.bufSize
      readSlice
        ( st0
            { _rl_capacity = Const.bufSize,
              _rl_buff = buff,
              _rl_dataPtr = buff
            }
        )
  where
    readSlice st@ReadLoopSt {..} =
      withForeignPtr _rl_dataPtr (\ptr -> hGetBufSome _rl_fileHandle ptr _rl_capacity) >>= \case
        n | n > 0 -> do
          let res = BufferSlice _rl_buff _rl_dataPtr n
          pure $
            Just
              ( res,
                st
                  { _rl_capacity = _rl_capacity - n,
                    _rl_dataPtr = _rl_dataPtr `plusForeignPtr` n
                  }
              )
        _ -> pure Nothing

readLoopInit :: Handle -> IO ReadLoopSt
readLoopInit h = do
  buff <- mallocForeignPtrBytes Const.bufSize
  pure (ReadLoopSt Const.bufSize buff buff h)

readLoop :: Text -> Handle -> (BufferSlice -> IO ()) -> IO ()
readLoop name fromH act = do
  let loop st0 = do
        readLoopStep st0 >>= \case
          Just (res, st) -> do
            act res
            loop st
          Nothing ->
            pure ()
  res <- tryIO (loop =<< readLoopInit fromH)
  case res of
    Left err -> hPutStrLn stderr (name <> " " <> show err)
    Right _ -> pure ()
