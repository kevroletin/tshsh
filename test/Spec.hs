{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Exception (evaluate)
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Lang.Coroutine.Folds
import Lang.Coroutine.Program
import Matcher.Text
import Protolude
import qualified Spec.Simulator
import System.IO
import Test.Hspec
import Test.Hspec.Expectations.Lens
import Test.QuickCheck
import Text.RawString.QQ
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

  describe "Coroutines lang" $ do
    it "finish x == x" $ do
      let (out, res) = accumOutputs @Int @Int (Finish (Right (10 :: Int)))
      out `shouldBe` []
      res `shouldHave` _Res . _Right . only 10

    it "andThen(nop, finish x) == x" $ do
      let (out, res) =
            accumOutputs @Int @Int
              ( AndThen
                  (Finish (Right ()))
                  (Finish (Right (10 :: Int)))
              )
      out `shouldBe` []
      res `shouldHave` _Res . _Right . only 10

    it "lam should pass value" $ do
      let (out, res) =
            accumOutputs @Int @Int
              ( AndThen
                  (Finish (Right (10 :: Int)))
                  (Lam $ \s -> Finish (Right s))
              )
      out `shouldBe` []
      res `shouldHave` _Res . _Right . only 10

    it "output outputs" $ do
      let (out, res) =
            accumOutputs @Int @Int
              ( Output 1 $
                  Output 2 $
                    Output 3 $
                      Finish (Right 4)
              )
      out `shouldBe` [1, 2, 3]
      res `shouldHave` _Res . _Right . only 4

    it "waitInput doesn't output" $ do
      let (out, res) =
            accumOutputs @Int @Int
              (WaitInput $ \i -> Output i (Finish (Right ())))
      out `shouldBe` []
      res `shouldHave` _Cont

    it "program executes until the frist waitInput" $ do
      let (out, res) =
            accumOutputs @Int @Int
              (Output 1 $ WaitInput $ \i -> Output i (Finish (Right ())))
      out `shouldBe` [1]
      res `shouldHave` _Cont

    it "feedInput" $ do
      let (out, res) =
            feedInputAccumOutputs @Int @Int
              10
              (Output 1 $ WaitInput $ \i -> Output i (Finish (Right ())))
      out `shouldBe` [1, 10]
      res `shouldHave` _Res . _Right . only ()

    it "program can have recursion" $ do
      let loop = WaitInput $ \i -> Output i loop

      accumProgram @Int @Int @() [1, 2, 3 :: Int] loop
        `shouldBe` ([1, 2, 3], Left "foldProgram: end of input")

    it "stateful program" $ do
      let loop 0 = Finish (Right ())
          loop n = WaitInput $ \i -> Output i (loop (n -1))

      accumProgram @Int @Int @() [1 ..] (loop 10)
        `shouldBe` ([1 .. 10], Right ())

    let echo = WaitInput $ \i -> Output i (Finish (Right ()))

    it "(andThen a b) terminates" $ do
      let p = AndThen echo echo

      accumProgram @Int @Int @() [1, 2, 3 :: Int] p
        `shouldBe` ([1, 2], Right ())

    it "andThen (andThen a b) c terminates" $ do
      let p = AndThen (AndThen echo echo) echo

      accumProgram @Int @Int @() [1, 2, 3 :: Int] p
        `shouldBe` ([1, 2, 3], Right ())

    it "andThen a (andThen a c) terminates" $ do
      let p = AndThen echo (AndThen echo echo)

      accumProgram @Int @Int @() [1, 2, 3 :: Int] p
        `shouldBe` ([1, 2, 3], Right ())

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

-- TODO: feeding input without consuming output throws errors
