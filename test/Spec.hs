{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Matcher.Seq.ByteString as MBS
import qualified Matcher.Seq.Text as MT
import Protolude
import qualified Spec.CPS
import qualified Spec.MonadT
import qualified Spec.Simulator
import qualified Spec.SimulatorM
import Test.Hspec
import Test.QuickCheck
import Prelude (String)

breakOnAll_ :: MT.Matcher -> Text -> Text -> [(Text, Text)]
breakOnAll_ m pat hay =
  case MT.matchStr m hay of
    MT.NoMatch _ -> []
    MT.Match {..} ->
      (match_prev, pat <> match_rest) :
      ( (\(a, b) -> (match_prev <> pat <> a, b))
          <$> breakOnAll_ match_matcher pat match_rest
      )

breakOnAll' :: Text -> Text -> [(Text, Text)]
breakOnAll' p = breakOnAll_ (MT.mkMatcher p) p

prop_sameAsTextImpl :: String -> String -> Property
prop_sameAsTextImpl (T.pack -> a) (T.pack -> b) =
  not (T.null a)
    ==> breakOnAll' a b == T.breakOnAll a b

dropEnd :: Int -> ByteString -> ByteString
dropEnd n str = BS.take (BS.length str - n) str

breakOnAllBs_ :: MBS.Matcher -> ByteString -> ByteString -> [(ByteString, ByteString)]
breakOnAllBs_ m pat hay =
  case MBS.matchStr m hay of
    MBS.NoMatch _ -> []
    MBS.Match m' len prev rest ->
      (dropEnd len prev, pat <> rest) :
      (first (prev <>) <$> breakOnAllBs_ m' pat rest)

breakOnAllBs' :: ByteString -> ByteString -> [(ByteString, ByteString)]
breakOnAllBs' p = breakOnAllBs_ (MBS.mkMatcher p) p

breakOnAllBs :: ByteString -> ByteString -> [(ByteString, ByteString)]
breakOnAllBs p str =
  let (prev, rest) = BS.breakSubstring p str
   in if BS.null str || BS.null rest
        then []
        else
          (prev, rest) :
          ( first (\x -> prev <> p <> x)
              <$> breakOnAllBs p (BS.drop (BS.length p) rest)
          )

prop_sameAsBsImpl :: String -> String -> Property
prop_sameAsBsImpl (C8.pack -> a) (C8.pack -> b) =
  not (BS.null a)
    ==> breakOnAllBs' a b == breakOnAllBs a b

main :: IO ()
main = hspec $ do
  describe "Matcher.Seq.Text" $ do
    it "breakOnAll is the same as Data.Text.breakOnAll" $
      property prop_sameAsTextImpl

  describe "Matcher.Seq.ByteString" $ do
    it "breakOnAll is the same as BS based breakOnAll" $
      property prop_sameAsBsImpl

    it "test manually" $ do
      breakOnAllBs' ":" "1:2:3" `shouldBe` [("1", ":2:3"), ("1:2", ":3")]
      breakOnAllBs' ":" "123" `shouldBe` []
      breakOnAllBs' ":" "---:---" `shouldBe` [("---", ":---")]
      breakOnAllBs' "123" ("112" <> "123" <> "112" <> "123") `shouldBe` [("112", "123" <> "112123"),
                                                                         ("112123112", "123")]

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
