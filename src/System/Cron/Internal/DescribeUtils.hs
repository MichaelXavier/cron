module System.Cron.Internal.DescribeUtils where

import Data.Char          (toUpper)
import Data.List.NonEmpty (NonEmpty)
import System.Cron.Types


leftPad :: Int -> String
leftPad n = if n < 10 then "0" ++ show n else show n


viewSF :: CronField -> Maybe SpecificField
viewSF (Field (SpecificField' s)) = Just s
viewSF _                          = Nothing


viewRD :: CronField -> Maybe RangeField
viewRD (Field (RangeField' r)) = Just r
viewRD _                       = Nothing


viewList :: CronField -> Maybe (NonEmpty BaseField)
viewList (ListField l) = Just l
viewList _             = Nothing


data Month = January | February | March     | April   | May      | June     |
             July    | August   | September | October | November | December
             deriving (Enum, Bounded, Show)


safeIntToMonth :: Int -> Month
safeIntToMonth = toEnum . subtract 1 . min 12 . max 1


data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
             deriving (Enum, Bounded, Show)


safeIntToWeekDay :: Int -> Weekday
safeIntToWeekDay = toEnum . subtract 1 . min 12 . max 1


allWords :: [String] -> String
allWords = unwords . filter (not . null)


time :: Int -> SpecificField -> String
time minute' hourF = leftPad (specificField hourF) ++ ":" ++ leftPad minute'


formatTime :: SpecificField -> SpecificField -> String
formatTime minuteF hourF = time (specificField minuteF) hourF


cap :: String -> String
cap []     = []
cap (x:xs) = toUpper x : xs


joinWords :: [String] -> String
joinWords [] = []
joinWords [x] = x
joinWords [x, y] = x ++ " and " ++ y
joinWords (x:xs) = x ++ ", " ++ joinWords xs
