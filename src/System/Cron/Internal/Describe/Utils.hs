module System.Cron.Internal.Describe.Utils
(
  viewHour
, viewMinute
, viewMinuteRange
, viewHourList
, allWords
, cap
, joinWords
) where

import           Data.Char               (toUpper)
import           Data.List.NonEmpty      (NonEmpty)
import           System.Cron.Types
import           System.Cron.Internal.Describe.Time

viewHour :: HourSpec -> Maybe Hour
viewHour = viewSpecificTime Hour . hourSpec


viewMinute :: MinuteSpec -> Maybe Minute
viewMinute = viewSpecificTime Minute . minuteSpec


viewMinuteRange :: MinuteSpec -> Maybe (Minute, Minute)
viewMinuteRange = viewRange . minuteSpec
  where viewRange (Field (RangeField' rf)) = Just (Minute $ rfBegin rf,
                                                   Minute $ rfEnd rf)
        viewRange _                        = Nothing


viewHourList :: HourSpec -> Maybe (NonEmpty BaseField)
viewHourList = viewList . hourSpec
  where viewList (ListField ne) = Just ne
        viewList _              = Nothing


viewSpecificTime :: (Int -> a) -> CronField -> Maybe a
viewSpecificTime f (Field (SpecificField' s)) = Just . f $ specificField s
viewSpecificTime _ _                          = Nothing


allWords :: [String] -> String
allWords = unwords . filter (not . null)


cap :: String -> String
cap []     = []
cap (x:xs) = toUpper x : xs


joinWords :: [String] -> String
joinWords [] = []
joinWords [x] = x
joinWords [x, y] = x ++ " and " ++ y
joinWords (x:xs) = x ++ ", " ++ joinWords xs
