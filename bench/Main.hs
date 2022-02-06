import Data.ByteString as BS
import Data.Strict.Tuple
import Gauge.Main
import Protolude
import qualified Spec.Simulator as S
import qualified Spec.SimulatorM as SM
import Tshsh.Lang.Coroutine.CPS (StepEnv (..))
import qualified Tshsh.Matcher as M
import qualified Tshsh.Matcher.Bracket as BrM
import qualified Tshsh.Matcher.Seq as SeqM
import qualified Tshsh.Stream as S
import Data.Time

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

countMatchesBr_ :: Int -> BrM.BracketMatcher -> ByteString -> Int
countMatchesBr_ !res m str
  | BS.null str = res
  | otherwise =
    case BrM.matchStr m str of
      S.ConsumerFinish m' _ rest _ -> countMatchesBr_ (res + 1) m' rest
      S.ConsumerContinue _ -> res

countMatchesBr :: BrM.BracketMatcher -> ByteString -> Int
countMatchesBr = countMatchesBr_ 0

countMatches_ :: Int -> SeqM.SeqMatcher -> ByteString -> Int
countMatches_ !res m str
  | BS.null str = res
  | otherwise =
    case SeqM.matchStr m str of
      S.ConsumerFinish m' _ rest _ -> countMatches_ (res + 1) m' rest
      S.ConsumerContinue _ -> res

countMatchesSeq :: SeqM.SeqMatcher -> ByteString -> Int
countMatchesSeq = countMatches_ 0

countMatchesStream_ :: Int -> S.StreamConsumer ByteString Int -> ByteString -> Int
countMatchesStream_ !res m str
  | BS.null str = res
  | otherwise =
    case S.consume m str of
      S.ConsumerFinish m' _ rest _ -> countMatchesStream_ (res + 1) m' rest
      S.ConsumerContinue _ -> res

countMatchesStream :: S.StreamConsumer ByteString Int -> ByteString -> Int
countMatchesStream = countMatchesStream_ 0

manySubstrings :: Int -> ByteString -> ByteString
manySubstrings n = BS.pack . Protolude.take n . cycle . mconcat . Protolude.inits . BS.unpack

zeros :: [Word8]
zeros = 0 : zeros

zeros100k :: ByteString
zeros100k = BS.pack $ Protolude.take (100 * 1024) zeros

defTime :: UTCTime
defTime = UTCTime (fromGregorian 2019 9 1) (timeOfDayToTime (TimeOfDay 15 13 0))

time :: StepEnv
time = StepEnv defTime

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
            whnf (countMatchesSeq (SeqM.mkSeqMatcher prompt)) str,
          bench "SeqM.Matcher seq" $
            whnf (countMatchesStream (M.mkSeqMatcher prompt)) str,
          bench "SeqM.Matcher bracket" $
            whnf (countMatchesBr (BrM.mkBracketMatcher "[[" "]]")) str,
          bench "SeqM.Matcher bracket" $
            whnf (countMatchesStream (M.mkBracketMatcher "[[" "]]")) str,
          bench "dummy BS.foldl'" $
            whnf (BS.foldl' (+) 0) str
        ],
      bgroup
        "many"
        [ bench "BS" $
            whnf (countMatchesBS longsubstring) str2,
          bench "SeqM.Matcher.Seq" $
            whnf (countMatchesSeq (SeqM.mkSeqMatcher longsubstring)) str2,
          bench "SeqM.Matcher seq" $
            whnf (countMatchesStream (M.mkSeqMatcher longsubstring)) str2,
          bench "dummy BS.foldl'" $
            whnf (BS.foldl' (+) 0) str2
        ],
      bgroup
        "Coroutines DSL"
        [ bench "CPS" $
            whnf
              (S.simulate time (() :!: S.syncEnv))
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
