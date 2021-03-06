{-# LANGUAGE TemplateHaskell #-}

module Tshsh.Lang.Coroutine.CPS.TH
  ( waitInputSecC,
    waitInputDefC,
    waitInputInfC,
    waitInputTimeC,
    waitInputSecC_,
    waitInputDefC_,
    waitInputInfC_,
    waitInputTimeC_,
    waitSecC_,
    waitTimeC_,
  )
where

import qualified Language.Haskell.TH as TH
import Protolude

callWithLoc :: [Char] -> TH.ExpQ
callWithLoc fname = do
  loc <- TH.location
  let (line, col) = TH.loc_start loc
  let pos = TH.loc_filename loc <> ": " <> show line <> ", " <> show col
  return
    ( TH.AppE
        (TH.VarE (TH.mkName fname))
        (TH.LitE (TH.StringL pos))
    )

waitInputSecC :: TH.ExpQ
waitInputSecC = callWithLoc "loc_waitInputSecC"

waitInputSecC_ :: TH.ExpQ
waitInputSecC_ = callWithLoc "loc_waitInputSecC_"

waitInputDefC :: TH.ExpQ
waitInputDefC = callWithLoc "loc_waitInputDefC"

waitInputDefC_ :: TH.ExpQ
waitInputDefC_ = callWithLoc "loc_waitInputDefC_"

waitInputInfC :: TH.ExpQ
waitInputInfC = callWithLoc "loc_waitInputInfC"

waitInputTimeC :: TH.ExpQ
waitInputTimeC = callWithLoc "loc_waitInputTimeC"

waitInputInfC_ :: TH.ExpQ
waitInputInfC_ = callWithLoc "loc_waitInputInfC_"

waitInputTimeC_ :: TH.ExpQ
waitInputTimeC_ = callWithLoc "loc_waitInputTimeC_"

waitSecC_ :: TH.ExpQ
waitSecC_ = callWithLoc "loc_waitSecC_"

waitTimeC_ :: TH.ExpQ
waitTimeC_ = callWithLoc "loc_waitTimeC_"
