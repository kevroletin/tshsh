{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as T
import Matcher.Text
import Test.Hspec
import Test.QuickCheck

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
