module Tshsh.Constants where

import Data.Time
import Protolude

bufSize :: Int
bufSize = 64 * 1024

minBufSize :: Int
minBufSize = 64

defaultProgramTimeoutSec :: NominalDiffTime
defaultProgramTimeoutSec = 5
