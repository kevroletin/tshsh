{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import Matcher.Result
import qualified Matcher.Seq.ByteString as MSBS
import qualified Matcher.Seq.Text as MST
import qualified Matcher.Bracket.Text as MBT
import Protolude
import qualified Spec.CPS
import qualified Spec.MonadT
import qualified Spec.Simulator
import qualified Spec.SimulatorM
import Test.Hspec
import Test.QuickCheck
import Prelude (String)
import Test.Hspec.Expectations.Lens
import Control.Lens
import qualified Data.BufferSlice as BufferSlice
import Data.BufferSlice (BufferSlice(..), SliceList(..))

resultBind :: MatchResult m a -> (m -> MatchResult m a) -> MatchResult m a
resultBind r@Match{} _ = r
resultBind (NoMatch m) f = f m

breakOnAll_ :: MST.Matcher -> Text -> Text -> [(Text, Text)]
breakOnAll_ m pat hay =
  case MST.matchStr m hay of
    NoMatch _ -> []
    Match {..} ->
      -- In the case of (a, b) = T.breakOn, b contains a pattern,
      -- in out case _match_prev contains a pattern
      let prev = T.dropEnd (T.length pat) _match_prev
          rest = pat <> _match_rest
       in (prev, rest) :
          ( (\(a, b) -> (prev <> pat <> a, b))
              <$> breakOnAll_ _match_matcher pat _match_rest
          )

breakOnAll' :: Text -> Text -> [(Text, Text)]
breakOnAll' p = breakOnAll_ (MST.mkMatcher p) p

prop_sameAsTextImpl :: String -> String -> Property
prop_sameAsTextImpl (T.pack -> a) (T.pack -> b) =
  not (T.null a)
    ==> breakOnAll' a b == T.breakOnAll a b

dropEnd :: Int -> ByteString -> ByteString
dropEnd n str = BS.take (BS.length str - n) str

breakOnAllBs_ :: MSBS.Matcher -> ByteString -> ByteString -> [(ByteString, ByteString)]
breakOnAllBs_ m pat hay =
  case MSBS.matchStr m hay of
    NoMatch _ -> []
    Match m' len prev rest ->
      (dropEnd len prev, pat <> rest) :
      (first (prev <>) <$> breakOnAllBs_ m' pat rest)

breakOnAllBs' :: ByteString -> ByteString -> [(ByteString, ByteString)]
breakOnAllBs' p = breakOnAllBs_ (MSBS.mkMatcher p) p

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

chopBs :: Int -> ByteString -> [ByteString]
chopBs n str
  | BS.null str = []
  | otherwise = BS.take n str : chopBs n (BS.drop n str)

sliceBs :: Int -> ByteString -> [BufferSlice]
sliceBs n str0 = fmap (BufferSlice.sliceFromByteString sliceId) (chopBs n str0)
  where sliceId = BS.toForeignPtr str0 ^. _1

main = hspec $ do
  describe "Buffer slices" $ do
    it "merge" $ do
      let xs = "1234"
      let (id, 0, _) = BS.toForeignPtr xs
      let Just ys = BufferSlice.mergeSlices (BufferSlice.sliceFromByteString id (BS.take 2 xs))
                                (BufferSlice.sliceFromByteString id (BS.drop 2 xs))
      BufferSlice.sliceToByteString ys `shouldBe` xs

    it "merge failure" $ do
      let xs = "1234"
      let ys = BS.copy xs
      let (id1, 0, _) = BS.toForeignPtr xs
      let (id2, 0, _) = BS.toForeignPtr ys
      let res = BufferSlice.mergeSlices (BufferSlice.sliceFromByteString id1 (BS.take 2 xs))
                            (BufferSlice.sliceFromByteString id2 (BS.drop 2 ys))
      res `shouldBe` Nothing

    it "merge zero" $ do
      let xs = "1234"
      let ys = ""
      let s = BufferSlice.sliceFromByteString (BS.toForeignPtr xs ^. _1) xs
      let sz = BufferSlice.sliceFromByteString (BS.toForeignPtr ys ^. _1) ys
      BufferSlice.mergeSlices s sz `shouldBe` Just s
      BufferSlice.mergeSlices sz s `shouldBe` Just s

    it "merge many" $ do
      let xs = "1234567890"
      let s = BufferSlice.sliceFromByteString (BS.toForeignPtr xs ^. _1) xs
      let (s1:ss) = sliceBs 2 xs
      let res = foldl' (\acc x -> join $ traverse (`BufferSlice.mergeSlices` x) acc) (Just s1) ss
      res `shouldBe` Just s

    it "merge slices from the same buffer" $ do
      let xs = "1234567890"
      let s = BufferSlice.sliceFromByteString (BS.toForeignPtr xs ^. _1) xs
      let res = foldl' BufferSlice.listAppendEnd BufferSlice.listEmpty (sliceBs 3 xs)
      res `shouldBe` SliceList [s]
      BufferSlice.listConcat res `shouldBe` xs

    it "merge slices from different buffers" $ do
      let xs = "1234567890"
      -- We know that slices produced by chopBs are located in memory one after the other.
      -- But because we assign different id's to each slice, BufferSlice.mergeSlices should recognize them
      -- as parts of different buffers (which might have different finalizers) and should refuse
      -- to merge them into a single slice.
      let ss = (\x -> BufferSlice.sliceFromByteString (BS.toForeignPtr (BS.copy x) ^. _1) x) <$> chopBs 3 xs
      let res@(SliceList rs) = foldl' BufferSlice.listAppendEnd BufferSlice.listEmpty ss
      length rs `shouldBe` length ss
      BufferSlice.listConcat res `shouldBe` xs

    it "slice take" $ do
      let xs = "1234567890"
      let s = BufferSlice.sliceFromByteString (BS.toForeignPtr xs ^. _1) xs
      BufferSlice.sliceToByteString (BufferSlice.sliceTake 4 s) `shouldBe` "1234"
      BufferSlice.sliceToByteString (BufferSlice.sliceTake 10 s) `shouldBe` "1234567890"
      BufferSlice.sliceToByteString (BufferSlice.sliceTake 100 s) `shouldBe` "1234567890"
      BufferSlice.sliceToByteString (BufferSlice.sliceTake 0 s) `shouldBe` ""
      BufferSlice.sliceToByteString (BufferSlice.sliceTake (-1) s) `shouldBe` ""

    it "slice take end" $ do
      let xs = "1234567890"
      let s = BufferSlice.sliceFromByteString (BS.toForeignPtr xs ^. _1) xs
      BufferSlice.sliceToByteString (BufferSlice.sliceTakeEnd 4 s) `shouldBe` "7890"
      BufferSlice.sliceToByteString (BufferSlice.sliceTakeEnd 10 s) `shouldBe` "1234567890"
      BufferSlice.sliceToByteString (BufferSlice.sliceTakeEnd 100 s) `shouldBe` "1234567890"
      BufferSlice.sliceToByteString (BufferSlice.sliceTakeEnd 0 s) `shouldBe` ""
      BufferSlice.sliceToByteString (BufferSlice.sliceTakeEnd (-1) s) `shouldBe` ""

    it "slice drop" $ do
      let xs = "1234567890"
      let s = BufferSlice.sliceFromByteString (BS.toForeignPtr xs ^. _1) xs
      BufferSlice.sliceToByteString (BufferSlice.sliceDrop 4 s) `shouldBe` "567890"
      BufferSlice.sliceToByteString (BufferSlice.sliceDrop 10 s) `shouldBe` ""
      BufferSlice.sliceToByteString (BufferSlice.sliceDrop 100 s) `shouldBe` ""
      BufferSlice.sliceToByteString (BufferSlice.sliceDrop 0 s) `shouldBe` "1234567890"
      BufferSlice.sliceToByteString (BufferSlice.sliceDrop (-1) s) `shouldBe` "1234567890"

    it "slice dropEnd" $ do
      let xs = "1234567890"
      let s = BufferSlice.sliceFromByteString (BS.toForeignPtr xs ^. _1) xs
      BufferSlice.sliceToByteString (BufferSlice.sliceDropEnd 4 s) `shouldBe` "123456"
      BufferSlice.sliceToByteString (BufferSlice.sliceDropEnd 10 s) `shouldBe` ""
      BufferSlice.sliceToByteString (BufferSlice.sliceDropEnd 100 s) `shouldBe` ""
      BufferSlice.sliceToByteString (BufferSlice.sliceDropEnd 0 s) `shouldBe` "1234567890"
      BufferSlice.sliceToByteString (BufferSlice.sliceDropEnd (-1) s) `shouldBe` "1234567890"

    it "drop" $ do
      let xs = "1234567890"
      let ss = (\x -> BufferSlice.sliceFromByteString (BS.toForeignPtr (BS.copy x) ^. _1) x) <$> chopBs 3 xs
      let res = foldl' BufferSlice.listAppendEnd BufferSlice.listEmpty ss
      BufferSlice.listConcat (BufferSlice.listDrop 5 res) `shouldBe` "67890"

    it "take" $ do
      let xs = "1234567890"
      let ss = (\x -> BufferSlice.sliceFromByteString (BS.toForeignPtr (BS.copy x) ^. _1) x) <$> chopBs 3 xs
      let res = foldl' BufferSlice.listAppendEnd BufferSlice.listEmpty ss
      BufferSlice.listConcat (BufferSlice.listTake 5 res) `shouldBe` "12345"

    it "dropEnd" $ do
      let xs = "1234567890"
      let ss = (\x -> BufferSlice.sliceFromByteString (BS.toForeignPtr (BS.copy x) ^. _1) x) <$> chopBs 3 xs
      let res = foldl' BufferSlice.listAppendEnd BufferSlice.listEmpty ss
      BufferSlice.listConcat (BufferSlice.listDropEnd 5 res) `shouldBe` "12345"

    it "takeEnd" $ do
      let xs = "1234567890"
      let ss = (\x -> BufferSlice.sliceFromByteString (BS.toForeignPtr (BS.copy x) ^. _1) x) <$> chopBs 3 xs
      let res = foldl' BufferSlice.listAppendEnd BufferSlice.listEmpty ss
      BufferSlice.listConcat (BufferSlice.listTakeEnd 5 res) `shouldBe` "67890"

  describe "Matcher.Seq.Text" $ do
    -- TODO: quickcheck generates quite useless input and doesn't catch errors,
    -- fix Arbitrary instances
    it "breakOnAll is the same as Data.Text.breakOnAll" $
      property prop_sameAsTextImpl

    it "test manually" $ do
      breakOnAll' ":" "1:2:3" `shouldBe` [("1", ":2:3"), ("1:2", ":3")]
      breakOnAll' ":" "123" `shouldBe` []
      breakOnAll' ":" "---:---" `shouldBe` [("---", ":---")]
      breakOnAll' "123" ("112" <> "123" <> "112" <> "123")
        `shouldBe` [ ("112", "123" <> "112123"),
                     ("112123112", "123")
                   ]

  describe "Matcher.Seq.ByteString" $ do
    it "breakOnAll is the same as BS based breakOnAll" $
      property prop_sameAsBsImpl

    it "test manually" $ do
      breakOnAllBs' ":" "1:2:3" `shouldBe` [("1", ":2:3"), ("1:2", ":3")]
      breakOnAllBs' ":" "123" `shouldBe` []
      breakOnAllBs' ":" "---:---" `shouldBe` [("---", ":---")]
      breakOnAllBs' "123" ("112" <> "123" <> "112" <> "123")
        `shouldBe` [ ("112", "123" <> "112123"),
                     ("112123112", "123")
                   ]

  describe "Matcher.Bracket.Text" $ do
    it "finds a match" $ do
      let res = MBT.matchStr (MBT.mkMatcher "[" "]") "prev[inside]rest"
      res `shouldHave` _Match
      res `shouldHave` match_prev . only "prev[inside]"
      res `shouldHave` match_rest . only "rest"
      res `shouldHave` match_matchLength . only 8

    it "feed input in chunks" $ do
      let res0 = MBT.matchStr (MBT.mkMatcher "[" "]") "prev[ins"
      res0 `shouldHave` _NoMatch
      let res = res0 `resultBind` (`MBT.matchStr` "ide]rest")
      res `shouldHave` _Match
      res `shouldHave` match_prev . only "ide]"
      res `shouldHave` match_rest . only "rest"
      res `shouldHave` match_matchLength . only 8

    it "feed input in many chunks" $ do
      let res0 = MBT.matchStr (MBT.mkMatcher "[" "]") ""
      res0 `shouldHave` _NoMatch
      let res = res0 `resultBind` (`MBT.matchStr` "pr")
                     `resultBind` (`MBT.matchStr` "e")
                     `resultBind` (`MBT.matchStr` "v[i")
                     `resultBind` (`MBT.matchStr` "nsi")
                     `resultBind` (`MBT.matchStr` "de]rest")
      res `shouldHave` _Match
      res `shouldHave` match_prev . only "de]"
      res `shouldHave` match_rest . only "rest"
      res `shouldHave` match_matchLength . only 8

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
