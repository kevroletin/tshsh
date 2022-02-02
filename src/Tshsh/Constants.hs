module Tshsh.Constants where

import Protolude

bufSize :: Int
bufSize = 64 * 1024

minBufSize :: Int
minBufSize = 64

defaultProgramTimeoutSec :: Int
defaultProgramTimeoutSec = 5
