import Data.ByteString as BS
import Data.Strict.Tuple
import Gauge.Main
import qualified Matcher.ByteString as SomeM
import Matcher.Result
import qualified Matcher.Seq.ByteString as SeqM
import Protolude
import qualified Spec.Simulator as S
import qualified Spec.SimulatorM as SM

-- BS.breakSubstring uses either robin-karp with a rolling hash, or an
-- optimization for small patterns with a byte shift and comparison of
-- integers. It doesn't have BS.skipUntil so it seem to perform worse
-- than our matcher on long strings with a few matches
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

countMatches_ :: Int -> SeqM.Matcher -> ByteString -> Int
countMatches_ !res m str
  | BS.null str = res
  | otherwise =
    case SeqM.matchStr m str of
      Match m' _ _ r -> countMatches_ (res + 1) m' r
      NoMatch _ -> res

countMatchesSeq :: SeqM.Matcher -> ByteString -> Int
countMatchesSeq = countMatches_ 0

countMatchesSomeM_ :: Int -> SomeM.SomeMatcher -> ByteString -> Int
countMatchesSomeM_ !res m str
  | BS.null str = res
  | otherwise =
    case SomeM.matchStr m str of
      Match m' _ _ r -> countMatchesSomeM_ (res + 1) m' r
      NoMatch _ -> res

countMatchesSomeM :: SomeM.SomeMatcher -> ByteString -> Int
countMatchesSomeM = countMatchesSomeM_ 0

manySubstrings :: Int -> ByteString -> ByteString
manySubstrings n = BS.pack . Protolude.take n . cycle . mconcat . Protolude.inits . BS.unpack

zeros :: [Word8]
zeros = 0 : zeros

zeros100k :: ByteString
zeros100k = BS.pack $ Protolude.take (100 * 1024) zeros

main :: IO ()
main = do
  let prompt = "[[prompt]]"
  let str = BS.concat $ Protolude.replicate 100 (prompt <> zeros100k)

  let longsubstring = "longsubstring"
  let str2 = manySubstrings (1024 * 1024) longsubstring

  defaultMain
    [ bgroup
        "long/a few"
        -- This test takes a long sequence (a few MB binary) with just a few
        -- occurrences of a pattern. Here we test how efficiently the matcher
        -- can skip irrelevant input. Such usage pattern might happen when a
        -- user of tshsh dumps a big file to the output.
        [ bench "BS" $
            whnf (countMatchesBS prompt) str,
          bench "SeqM.Matcher.Seq" $
            whnf (countMatchesSeq (SeqM.mkMatcher prompt)) str,
          bench "SeqM.Matcher seq" $
            whnf (countMatchesSomeM (SomeM.mkSeqMatcher prompt)) str,
          bench "SeqM.Matcher bracket" $
            whnf (countMatchesSomeM (SomeM.mkBracketMatcher "[[" "]]")) str,
          bench "dummy BS.foldl'" $
            whnf (BS.foldl' (+) 0) str
        ],
      bgroup
        "many"
        [ bench "BS" $
            whnf (countMatchesBS longsubstring) str2,
          bench "SeqM.Matcher.Seq" $
            whnf (countMatchesSeq (SeqM.mkMatcher longsubstring)) str2,
          bench "SeqM.Matcher seq" $
            whnf (countMatchesSomeM (SomeM.mkSeqMatcher longsubstring)) str2,
          bench "dummy BS.foldl'" $
            whnf (BS.foldl' (+) 0) str2
        ],
      bgroup
        "Coroutines DSL"
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
