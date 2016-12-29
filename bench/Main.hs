{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Criterion
import           Criterion.Main
import           Data.Attoparsec.Text (Parser, parseOnly)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time
-------------------------------------------------------------------------------
import           System.Cron
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain
  [ parserBenchmarks
  , scheduleMatchesBenchmarks
  , nextMatchBenchmarks
  , serializeBenchmarks
  , describeBenchmarks
  ]


-------------------------------------------------------------------------------
parserBenchmarks :: Benchmark
parserBenchmarks = bgroup "parsers"
  [ parserBench "cronSchedule" cronSchedule cronScheduleText
  , parserBench "cronScheduleLoose" cronScheduleLoose cronScheduleText
  , parserBench "crontab" crontab cronTabText
  , bgroup "crontabEntry"
      [ parserBench "schedule" crontabEntry cronScheduleText
      , parserBench "env set" crontabEntry envSetText
      ]
  ]


-------------------------------------------------------------------------------
scheduleMatchesBenchmarks :: Benchmark
scheduleMatchesBenchmarks = bgroup "scheduleMatches"
  [
    bench "match" (whnf (scheduleMatches weekly) matchingTime)
  , bench "no match" (whnf (scheduleMatches weekly) nonMatchingTime)
  ]
  where
    matchingTime = mkTime 2016 2 14 0 0 0
    nonMatchingTime = mkTime 2016 2 15 0 0 0


-------------------------------------------------------------------------------
nextMatchBenchmarks :: Benchmark
nextMatchBenchmarks =
  bench "nextMatch" (whnf (nextMatch weekly) now)
  where
    now = mkTime 2016 2 14 0 0 0

-------------------------------------------------------------------------------
serializeBenchmarks :: Benchmark
serializeBenchmarks = bgroup "serialization"
  [
    bench "cronSchedule" (whnf serializeCronSchedule weekly)
  , bench "crontab" (whnf serializeCrontab exampleCrontab)
  ]


-------------------------------------------------------------------------------
describeBenchmarks :: Benchmark
describeBenchmarks = bgroup "description"
  [
    bgroup "verbose" [
      bench "simple" (whnf (describe Verbose) simpleCronSchedule)
    , bench "complicated" (whnf (describe Verbose) complexCronSchedule )
    ]
  , bgroup "non-verbose" [
      bench "simple" (whnf (describe NotVerbose) simpleCronSchedule)
    , bench "complicated" (whnf (describe NotVerbose) complexCronSchedule)
    ]
  ]


-------------------------------------------------------------------------------
parserBench :: String -> Parser a -> Text -> Benchmark
parserBench n parser txt = bench n (whnf (parseOnly parser) txt)


-------------------------------------------------------------------------------
cronScheduleText :: Text
cronScheduleText = "*/2 * 3 * 4,5,6"


-------------------------------------------------------------------------------
envSetText :: Text
envSetText = "FOO=BAR"

-------------------------------------------------------------------------------
cronTabText :: Text
cronTabText = T.unlines (concat (zipWith (\x y -> [x,y]) (replicate 50 cronScheduleText) (repeat envSetText)))


-------------------------------------------------------------------------------
mkCronSchedule :: Text -> CronSchedule
mkCronSchedule t = let (Right cs) = parseCronSchedule t in cs


-------------------------------------------------------------------------------
simpleCronSchedule :: CronSchedule
simpleCronSchedule = mkCronSchedule "1 2 3 * *"


-------------------------------------------------------------------------------
complexCronSchedule :: CronSchedule
complexCronSchedule = mkCronSchedule "1-10 3 1-20/2 * 3,5"


-------------------------------------------------------------------------------
exampleCrontab :: Crontab
exampleCrontab = Crontab (concat (zipWith (\x y -> [x,y]) (replicate 50 cmd) (repeat envSet)))
  where
    cmd = CommandEntry weekly (CronCommand "do something")
    envSet = EnvVariable "FOO" "BAR"


-------------------------------------------------------------------------------
mkTime
    :: Integer
    -> Int
    -> Int
    -> DiffTime
    -> DiffTime
    -> DiffTime
    -> UTCTime
mkTime y m d hr mn s = UTCTime day time
  where day = fromGregorian y m d
        time = s + 60 * mn + 60 * 60 * hr
