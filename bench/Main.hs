import Data.ByteString as BS
import Data.Strict.Tuple
import qualified Data.Text as T
import Gauge.Main
import Matcher.Seq.ByteString
import Protolude
import qualified Spec.Simulator as S
import qualified Spec.SimulatorM as SM
import System.Process

-- BS.breakSubstring uses either robin-karp with a rolling hash, or a
-- optimization for small patterns with a byte shift and comparison of
-- integers. It doesn't have BS.skipUntil so it seem to perform worse
-- than our matcher on long strings without any matches.
countMatchesBS_ :: Int -> ByteString -> ByteString -> Int
countMatchesBS_ !res p str
  | BS.null str = res
  | otherwise =
    let (_, b) = BS.breakSubstring p str
     in if BS.null b
          then res
          else countMatchesBS_ (res + 1) p (BS.drop (BS.length p) b)

countMatchesBS :: ByteString -> ByteString -> Int
countMatchesBS = countMatchesBS_ 0

countMatches_ :: Int -> Matcher -> ByteString -> Int
countMatches_ !res m str
  | BS.null str = res
  | otherwise =
    case matchStr m str of
      Match m' _ _ r -> countMatches_ (res + 1) m' r
      NoMatch _ -> res

countMatchesSeq :: Matcher -> ByteString -> Int
countMatchesSeq = countMatches_ 0

main :: IO ()
main = do
  fname <- T.strip . T.pack <$> readProcess "sh" ["-c", "find -type f -name tshsh-exe | head -n 1"] ""
  str <- withFile (T.unpack fname) ReadMode BS.hGetContents

  defaultMain
    [ bgroup
        "Matcher.Seq"
        -- This test takes a big sequence (a few MB binary) with just a few
        -- occurrences of a pattern. Here we test how efficiently the matcher
        -- can skip irrelevant input. Such usage pattern might happen when a
        -- user of tshsh dumps a big file to the output.
        [ bench "BS" $
            whnf (countMatchesBS "word") str,
          bench "Matcher.Seq" $
            whnf (countMatchesSeq (mkMatcher "word")) str
        ],
      bgroup
        "lang"
        [ bench "CPS" $
            whnf
              (S.simulate (() :!: S.syncEnv))
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
