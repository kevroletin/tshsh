{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

import Control.Monad
import Control.Monad.Except
import Gauge.Main
import qualified Lang.Coroutine.Test as Test
import Protolude
import qualified Spec.Simulator as S
import qualified Spec.SimulatorM as SM
import System.Environment
import System.Random
import Prelude

main :: IO ()
main =
  defaultMain
    [ bgroup
        "lang"
        [ bench "CPS" $
            whnf
              (S.simulate S.syncEnv)
              [ (S.Shell_1, "env\n", "a=1\nb=2\n"),
                (S.Shell_1, "pwd\n", "/root\n"),
                (S.Shell_2, "export a=1\n", ""),
                (S.Shell_2, "export b=2\n", ""),
                (S.Shell_2, "cd '/root'\n", "")
              ],
          bench "Monadic" $
            whnf
              (SM.simulate SM.syncEnv)
              [ (SM.Shell_1, "env\n", "a=1\nb=2\n"),
                (SM.Shell_1, "pwd\n", "/root\n"),
                (SM.Shell_2, "export a=1\n", ""),
                (SM.Shell_2, "export b=2\n", ""),
                (SM.Shell_2, "cd '/root'\n", "")
              ]
        ]
    ]
