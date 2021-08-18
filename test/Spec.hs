{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Lens
import Tshsh.Data.BufferSlice (BufferSlice (..), SliceList (..))
import qualified Tshsh.Data.BufferSlice as BufferSlice
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Internal as BS
import qualified Data.Text as T
import qualified Tshsh.Matcher.Bracket as BrM
import Tshsh.Stream
import qualified Tshsh.Matcher.Seq as SeqM
import Protolude
import qualified Spec.CPS
import qualified Spec.MonadT
import qualified Spec.Muxer
import qualified Spec.Simulator
import qualified Spec.SimulatorM
import Test.Hspec
import Test.Hspec.Expectations.Lens
import Test.QuickCheck
import Prelude (String)

resultBind :: ConsumerResult m arr a -> (m -> ConsumerResult m arr a) -> ConsumerResult m arr a
resultBind r@ConsumerFinish {} _ = r
resultBind (ConsumerContinue m) f = f m

dropEnd :: Int -> ByteString -> ByteString
dropEnd n str = BS.take (BS.length str - n) str

breakOnAllSeq_ :: SeqM.SeqMatcher -> ByteString -> ByteString -> [(ByteString, ByteString)]
breakOnAllSeq_ m pat hay =
  case SeqM.matchStr m hay of
    ConsumerContinue _ -> []
    ConsumerFinish m' prev rest len ->
      (dropEnd len prev, pat <> rest) :
      (first (prev <>) <$> breakOnAllSeq_ m' pat rest)

breakOnAllSeq :: ByteString -> ByteString -> [(ByteString, ByteString)]
breakOnAllSeq p = breakOnAllSeq_ (SeqM.mkSeqMatcher p) p

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
    ==> breakOnAllSeq a b == breakOnAllBs a b

main :: IO ()
chopBs :: Int -> ByteString -> [ByteString]
chopBs n str
  | BS.null str = []
  | otherwise = BS.take n str : chopBs n (BS.drop n str)

sliceBs :: Int -> ByteString -> [BufferSlice]
sliceBs n str0 = fmap (BufferSlice.sliceFromByteString sliceId) (chopBs n str0)
  where
    sliceId = BS.toForeignPtr str0 ^. _1

main = hspec $ do
  describe "Buffer slices" $ do
    it "merge" $ do
      let xs = "1234"
      let (id, 0, _) = BS.toForeignPtr xs
      let Just ys =
            BufferSlice.mergeSlices
              (BufferSlice.sliceFromByteString id (BS.take 2 xs))
              (BufferSlice.sliceFromByteString id (BS.drop 2 xs))
      BufferSlice.sliceToByteString ys `shouldBe` xs

    it "merge failure" $ do
      let xs = "1234"
      let ys = BS.copy xs
      let (id1, 0, _) = BS.toForeignPtr xs
      let (id2, 0, _) = BS.toForeignPtr ys
      let res =
            BufferSlice.mergeSlices
              (BufferSlice.sliceFromByteString id1 (BS.take 2 xs))
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
      let (s1 : ss) = sliceBs 2 xs
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

  describe "Matcher.Seq.ByteString" $ do
    it "breakOnAll is the same as BS based breakOnAll" $
      property prop_sameAsBsImpl

    it "test manually" $ do
      breakOnAllSeq ":" "1:2:3" `shouldBe` [("1", ":2:3"), ("1:2", ":3")]
      breakOnAllSeq ":" "123" `shouldBe` []
      breakOnAllSeq ":" "---:---" `shouldBe` [("---", ":---")]
      breakOnAllSeq "123" ("112" <> "123" <> "112" <> "123")
        `shouldBe` [ ("112", "123" <> "112123"),
                     ("112123112", "123")
                   ]

  describe "Matcher.Bracket.ByteString" $ do
    it "finds a match" $ do
      let res = BrM.matchStr (BrM.mkBracketMatcher "[" "]") "prev[inside]rest"
      res `shouldHave` _ConsumerFinish
      res `shouldHave` cs_prev . only "prev[inside]"
      res `shouldHave` cs_rest . only "rest"
      res `shouldHave` cs_result . only 8

    it "feed input in chunks" $ do
      let res0 = BrM.matchStr (BrM.mkBracketMatcher "[" "]") "prev[ins"
      res0 `shouldHave` _ConsumerContinue
      let res = res0 `resultBind` (`BrM.matchStr` "ide]rest")
      res `shouldHave` _ConsumerFinish
      res `shouldHave` cs_prev . only "ide]"
      res `shouldHave` cs_rest . only "rest"
      res `shouldHave` cs_result . only 8

    it "feed input in many chunks" $ do
      let res0 = BrM.matchStr (BrM.mkBracketMatcher "[" "]") ""
      res0 `shouldHave` _ConsumerContinue
      let res =
            res0 `resultBind` (`BrM.matchStr` "pr")
              `resultBind` (`BrM.matchStr` "e")
              `resultBind` (`BrM.matchStr` "v[i")
              `resultBind` (`BrM.matchStr` "nsi")
              `resultBind` (`BrM.matchStr` "de]rest")
      res `shouldHave` _ConsumerFinish
      res `shouldHave` cs_prev . only "de]"
      res `shouldHave` cs_rest . only "rest"
      res `shouldHave` cs_result . only 8

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

  describe "Muxer" Spec.Muxer.spec
