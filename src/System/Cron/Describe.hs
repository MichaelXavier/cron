{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Cron.Describe
(
  -- Verbosity(..)
-- , displaySchedule
  describeTime
, describe
-- , description
-- , minuteDescriptor
) where

import Control.Monad
import System.Cron.Types
-- import Data.Char                                (toUpper)
import Data.List.NonEmpty                       (NonEmpty (..))
-- import qualified Data.List.NonEmpty as N hiding (NonEmpty (..))
import Data.List                                (intercalate)
import Data.Maybe                               (isJust, fromJust)
-- import Data.Text                                (Text)
-- import qualified Data.Text as T hiding          (Text)

leftPad :: Int -> String
leftPad n = if n < 10 then "0" ++ show n else show n

toHour :: Int -> String
toHour n = leftPad n ++ ":00"

viewSF :: CronField -> Maybe SpecificField
viewSF (Field (SpecificField' s)) = Just s
viewSF _                          = Nothing

viewRD :: CronField -> Maybe RangeField
viewRD (Field (RangeField' r)) = Just r
viewRD _                       = Nothing

viewList :: CronField -> Maybe (NonEmpty BaseField)
viewList (ListField l) = Just l
viewList _             = Nothing

data Descriptor = Descriptor {
    listPrefix       :: String
  , listSuffix       :: Maybe String
  , listRangeDisplay :: Int -> Int -> String
  , stepPrefix       :: Int -> String
  , stepSuffixSpec   :: BaseField -> Maybe String
  , displaySpec      :: BaseField -> String
  , describeItem     :: Int -> String
  }

minuteDescriptor :: Descriptor
minuteDescriptor = Descriptor{
    listPrefix       = "at"
  , listSuffix       = Just "minutes past the hour"
  , listRangeDisplay = (\b e -> show b ++ " through " ++ show e)
  , stepPrefix       = (\n -> "every " ++ show n ++ " minutes")
  , stepSuffixSpec   = stepSuffix
  , displaySpec      = display
  , describeItem     = show
  }
  where dispRange b e = "minutes " ++ show b ++ " through " ++ show e ++ " past the hour"
        stepSuffix (RangeField' rf)   = Just $ dispRange (rfBegin rf) (rfEnd rf)
        stepSuffix (SpecificField' s)
          | n == 0    = Nothing
          | otherwise = Just $ "starting at " ++ show n ++ " minutes past the hour"
          where n = specificField s
        stepSuffix Star               = Nothing
        display (RangeField' rf)   = dispRange (rfBegin rf) (rfEnd rf)
        display (SpecificField' s) = "at " ++ show (specificField s) ++ " minutes past the hour"
        display Star               = "every minute"

hourDescriptor :: Descriptor
hourDescriptor = Descriptor{
    listPrefix       = "at"
  , listSuffix       = Nothing
  , listRangeDisplay = (\b e -> dispItem b ++ " through " ++ dispItem e)
  , stepPrefix       = (\n -> "every " ++ show n ++ " hours")
  , stepSuffixSpec   = stepSuffix
  , displaySpec      = display
  , describeItem     = dispItem
  }
  where dispRange b e = "between " ++ toHour b ++ " and " ++ toHour e
        dispItem n = leftPad n ++ ":00"
        stepSuffix (RangeField' rf)   = Just $ dispRange (rfBegin rf) (rfEnd rf)
        stepSuffix (SpecificField' s)
          | n == 0    = Nothing
          | otherwise = Just $ "starting at " ++ dispItem n
          where n = specificField s
        stepSuffix Star            = Nothing
        display (RangeField' rf)   = dispRange (rfBegin rf) (rfEnd rf)
        display (SpecificField' s) = "at " ++ dispItem (specificField s)
        display Star               = "every hour"

describe :: CronField -> Descriptor -> String
describe (Field f)       d = displaySpec d f
describe (StepField' sf) d =
  stepPrefix d (sfStepping sf) ++ maybe "" (", " ++) (stepSuffixSpec d $ sfField sf)
describe (ListField ls)  d =
  case describeListFields describeBF ls of
    Left  s -> s
    Right s -> listPrefix d ++ " " ++ maybe s ((s ++ ", ") ++) (listSuffix d)
  where
    describeBF Star               = displaySpec d Star
    describeBF (RangeField' rf)   = listRangeDisplay d (rfBegin rf) (rfBegin rf)
    describeBF (SpecificField' s) = describeItem d $ specificField s

describeListFields :: (BaseField -> String) -> NonEmpty BaseField -> Either String String
describeListFields f (l :| ls) =
  fmap (intercalate ", ") . foldM describeF [] $ reverse (l:ls)
  where describeF _ Star = Left $ f Star
        describeF e bf   = Right $ f bf : e

describeTime :: MinuteSpec -> HourSpec -> String
describeTime (viewSF . minuteSpec -> Just m) (viewSF . hourSpec -> Just h) = "at " ++ formatTime m h
describeTime (viewRD . minuteSpec -> Just m) (viewSF . hourSpec -> Just h) = "every minute between " ++ time (rfBegin m) h ++ " and " ++ time (rfEnd m) h
describeTime (viewSF . minuteSpec -> Just m) (viewList . hourSpec -> Just h)  = describeMultHours m h
describeTime (minuteSpec -> m) (hourSpec -> h) =
  describe m minuteDescriptor ++ ", " ++ describe h hourDescriptor

describeMultHours :: SpecificField -> NonEmpty BaseField -> String
describeMultHours minuteSF ls@(bf :| bfs)
  | all isJust formattedTimes = "at " ++ intercalate ", " (map fromJust formattedTimes)
  | otherwise = describe (Field (SpecificField' minuteSF)) minuteDescriptor ++
                ", " ++
                describe (ListField ls) hourDescriptor
  where formattedTimes = map formatBaseField (bf : bfs)
        formatBaseField (SpecificField' s) = Just $ formatTime minuteSF s
        formatBaseField _                  = Nothing

time :: Int -> SpecificField -> String
time minute' hourF = leftPad (specificField hourF) ++ ":" ++ leftPad minute'

formatTime :: SpecificField -> SpecificField -> String
formatTime minuteF hourF = time (specificField minuteF) hourF
