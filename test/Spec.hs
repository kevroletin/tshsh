{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import Protolude
import Prelude (String)

import System.IO
import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as T
import Matcher.Text

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Expectations.Lens
import Control.Lens

import Lang.Coroutine.Program
import Lang.Coroutine.Folds

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
      property $ prop_sameAsTextImpl

  describe "Lang" $ do
    it "finish x == x" $ do
       let (out, res) = accumOutputs @Int @Int (Finish (10 :: Int))
       out `shouldBe` []
       res `shouldHave` _Left . only 10

    it "andThen(nop, finish x) == x" $ do
       let (out, res) = accumOutputs @Int @Int
                          ( AndThen
                             (Finish ())
                             (Finish (10 :: Int)))
       out `shouldBe` []
       res `shouldHave` _Left . only 10

    it "lam should pass value" $ do
       let (out, res) = accumOutputs @Int @Int
                          ( AndThen
                             (Finish (10 :: Int))
                             (Lam $ \s -> Finish s))
       out `shouldBe` []
       res `shouldHave` _Left . only 10

    it "output outputs" $ do
       let (out, res) = accumOutputs @Int @Int
                        ( Output 1 $
                            Output 2 $
                              Output 3 $
                                Finish 4)
       out `shouldBe` [1, 2, 3]
       res `shouldHave` _Left . only 4

    it "waitInput doesn't output" $ do
       let (out, res) = accumOutputs @Int @Int
                        ( WaitInput $ \i -> Output i (Finish ()))
       out `shouldBe` []
       res `shouldHave` _Right

    it "program executes until the frist waitInput" $ do
       let (out, res) = accumOutputs @Int @Int
                        ( Output 1 $ WaitInput $ \i -> Output i (Finish ()))
       out `shouldBe` [1]
       res `shouldHave` _Right

    it "feedInput" $ do
       let (out, res) = feedInputAccumOutSafe @Int @Int
                        10
                        ( Output 1 $ WaitInput $ \i -> Output i (Finish ()))
       out `shouldBe` [1, 10]
       res `shouldHave` _Left

    it "program can have recursion" $ do
       let loop = WaitInput $ \i -> Output i loop

       accumFoldProgram @Int @Int @() [1, 2, 3 :: Int] loop
         `shouldBe` ([1, 2, 3], Nothing)

    it "stateful program" $ do
       let loop 0 = Finish ()
           loop n = WaitInput $ \i -> Output i (loop (n-1))

       accumFoldProgram @Int @Int @() [1..] (loop 10)
         `shouldBe` ([1 .. 10], Just ())

    let echo = WaitInput $ \i -> Output i (Finish ())

    it "(andThen a b) terminates" $ do
       let p = AndThen echo echo

       accumFoldProgram @Int @Int @() [1, 2, 3 :: Int] p
         `shouldBe` ([1, 2], Just ())

    it "andThen (andThen a b) c terminates" $ do
       let p = AndThen (AndThen echo echo) echo

       accumFoldProgram @Int @Int @() [1, 2, 3 :: Int] p
         `shouldBe` ([1, 2, 3], Just ())

    it "andThen a (andThen a c) terminates" $ do
       let p = AndThen echo (AndThen echo echo)

       accumFoldProgram @Int @Int @() [1, 2, 3 :: Int] p
         `shouldBe` ([1, 2, 3], Just ())

    -- TODO: test corner cases with unconsumed input
    -- TODO: test monadic folds
