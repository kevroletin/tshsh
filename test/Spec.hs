{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.Text as T
import Matcher.Seq.Text
import Protolude
import qualified Spec.CPS
import qualified Spec.MonadT
import qualified Spec.Simulator
import qualified Spec.SimulatorM
import Test.Hspec
import Test.QuickCheck
import Prelude (String)

breakOnAll_ :: Matcher -> Text -> Text -> [(Text, Text)]
breakOnAll_ m pat hay =
  case matchStr m hay of
    NoMatch _ -> []
    Match {..} ->
      (match_prev, pat <> match_rest) :
      ( (\(a, b) -> (match_prev <> pat <> a, b))
          <$> breakOnAll_ match_matcher pat match_rest
      )

breakOnAll' :: Text -> Text -> [(Text, Text)]
breakOnAll' p = breakOnAll_ (mkMatcher p) p

prop_sameAsTextImpl :: String -> String -> Property
prop_sameAsTextImpl (T.pack -> a) (T.pack -> b) =
  not (T.null a)
    ==> breakOnAll' a b == T.breakOnAll a b

main :: IO ()
main = hspec $ do
  describe "Matcher.Text" $ do
    it "breakOnAll is the same as Data.Text.breakOnAll" $
      property prop_sameAsTextImpl

  describe "CPS lang" Spec.CPS.spec

  describe "Monadic lang" Spec.MonadT.spec

  describe "simulate real world behaviour" $ do
    it "simulate environmet sync" $ do
      let res =
            T.intercalate
              "\n"
              [ "--- Shell_1 ---",
                " $ env",
                "a=1",
                "b=2",
                " $ pwd",
                "/root",
                " $ ",
                "--- Shell_2 ---",
                " $ export a=1",
                " $ export b=2",
                " $ cd '/root'",
                " $ "
              ]
      Spec.Simulator.simulateEnvSync `shouldBe` Right res
      Spec.SimulatorM.simulateEnvSync `shouldBe` Right res
