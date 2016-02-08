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
-------------------------------------------------------------------------------
import           System.Cron.Parser
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain
  [ parserBench "cronSchedule" cronSchedule cronScheduleText
  , parserBench "cronScheduleLoose" cronScheduleLoose cronScheduleText
  , parserBench "crontab" crontab cronTabText
  , bgroup "crontabEntry"
      [ parserBench "schedule" crontabEntry cronScheduleText
      , parserBench "env set" crontabEntry envSetText
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
