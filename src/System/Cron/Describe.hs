{-# LANGUAGE ViewPatterns #-}

module System.Cron.Describe
(
  describeTime
, describe
) where

import Control.Monad
import System.Cron.Types
import Data.List.NonEmpty                       (NonEmpty (..))
import Data.List                                (intercalate)
import Data.Maybe                               (isJust, fromJust)
import System.Cron.Internal.DescribeUtils

data Descriptor = Descriptor {
    pluralDescription   :: String
  , singularDescription :: String
  , rangePrefix         :: String
  , rangeSuffix         :: String
  , rangeJoiner         :: String
  , displayItem         :: Int -> String
  , specificPrefix      :: String
  , specificSuffix      :: String
  , stepSpecificSuffix  :: Int -> Maybe String
  , listPrefix          :: String
  , listSuffix          :: Maybe String
  }

minuteDescriptor :: Descriptor
minuteDescriptor = Descriptor {
    pluralDescription = "minutes"
  , singularDescription = "minute"
  , rangePrefix = "minutes"
  , rangeSuffix = "past the hour"
  , rangeJoiner = "through"
  , displayItem = show
  , specificPrefix = "at"
  , specificSuffix = "minutes past the hour"
  , stepSpecificSuffix = sss
  , listPrefix = "at"
  , listSuffix = Nothing
  }
  where sss n = if n == 0 then Nothing else Just $ "starting at " ++ show n ++ " minutes past the hour"

hourDescriptor :: Descriptor
hourDescriptor = Descriptor {
    pluralDescription = "hours"
  , singularDescription = "hour"
  , rangePrefix = "between"
  , rangeSuffix = ""
  , rangeJoiner = "and"
  , displayItem = toHour
  , specificPrefix = "at"
  , specificSuffix = ""
  , stepSpecificSuffix = sss
  , listPrefix = "at"
  , listSuffix = Nothing
  }
  where toHour n = leftPad n ++ ":00"
        sss n = if n == 0 then Nothing else Just $ "starting at " ++ toHour n

domDescriptor :: Descriptor
domDescriptor = Descriptor {
    pluralDescription = "days"
  , singularDescription = "day"
  , rangePrefix = "between days"
  , rangeSuffix = "of the month"
  , rangeJoiner = "and"
  , displayItem = show
  , specificPrefix = "on day"
  , specificSuffix = "of the month"
  , stepSpecificSuffix = sss
  , listPrefix = "on days"
  , listSuffix = Just "of the month"
  }
  where sss n = Just $ "starting on day " ++ show n ++ " of the month"

monthDescriptor :: Descriptor
monthDescriptor = Descriptor {
    pluralDescription = "months"
  , singularDescription = "month"
  , rangePrefix = ""
  , rangeSuffix = ""
  , rangeJoiner = "through"
  , displayItem = toMonth
  , specificPrefix = "only in"
  , specificSuffix = ""
  , stepSpecificSuffix = sss
  , listPrefix = "only in"
  , listSuffix = Nothing
  }
  where toMonth = show . safeIntToMonth
        sss n = if n == 1 then Nothing else Just $ toMonth n ++ " through " ++ toMonth 12

dowDescriptor :: Descriptor
dowDescriptor = Descriptor {
    pluralDescription = "days of the week"
  , singularDescription = "day of the week"
  , rangePrefix = ""
  , rangeSuffix = ""
  , rangeJoiner = "through"
  , displayItem = toWeekday
  , specificPrefix = "only on"
  , specificSuffix = ""
  , stepSpecificSuffix = sss
  , listPrefix = "only on"
  , listSuffix = Nothing
  }
  where toWeekday = show . safeIntToWeekDay
        -- FIXME
        sss n = if n == 0 then Nothing else Just $ toWeekday n ++ " through " ++ toWeekday 6

describeRange :: RangeField -> Descriptor -> String
describeRange rf d = allWords [rangePrefix d, displayItem d (rfBegin rf), rangeJoiner d, displayItem d (rfEnd rf), rangeSuffix d]

describeBaseField :: Descriptor -> BaseField -> String
describeBaseField d (RangeField' rf)   = describeRange rf d
describeBaseField d (SpecificField' s) = allWords [specificPrefix d, displayItem d (specificField s), specificSuffix d]
describeBaseField d Star               = "every " ++ singularDescription d

describeListFields :: (BaseField -> String) -> NonEmpty BaseField -> Either String String
describeListFields f (l :| ls) =
  fmap (intercalate ", ") . foldM describeF [] $ reverse (l:ls)
  where describeF _ Star = Left $ f Star
        describeF e bf   = Right $ f bf : e

description :: CronField -> Descriptor -> String
description (Field f) d = describeBaseField d f

description (StepField' sf) d =
  stepPrefix ++ maybe "" (", " ++) (stepSuffix $ sfField sf)
  where
    stepPrefix = unwords ["every", show (sfStepping sf), pluralDescription d]
    stepSuffix Star              = Nothing
    stepSuffix (RangeField' rf)  = Just $ describeRange rf d
    stepSuffix (SpecificField' s) = stepSpecificSuffix d $ specificField s

description (ListField ls) d =
  case describeListFields describeBF ls of
    Left  s -> s
    Right s -> unwords [listPrefix d, maybe s ((s ++ ", ") ++) (listSuffix d)]
  where
    describeBF Star               = "every " ++ singularDescription d
    describeBF (RangeField' rf)   = unwords [displayItem d (rfBegin rf), "through", displayItem d (rfEnd rf)]
    describeBF (SpecificField' s) = displayItem d $ specificField s

describeTime :: MinuteSpec -> HourSpec -> String
describeTime (viewSF . minuteSpec -> Just m) (viewSF . hourSpec -> Just h) = "at " ++ formatTime m h
describeTime (viewRD . minuteSpec -> Just m) (viewSF . hourSpec -> Just h) = "every minute between " ++ time (rfBegin m) h ++ " and " ++ time (rfEnd m) h
describeTime (viewSF . minuteSpec -> Just m) (viewList . hourSpec -> Just h)  = describeMultHours m h
describeTime (minuteSpec -> m) (hourSpec -> h) =
  description m minuteDescriptor ++ ", " ++ description h hourDescriptor

describeMultHours :: SpecificField -> NonEmpty BaseField -> String
describeMultHours minuteSF ls@(bf :| bfs)
  | all isJust formattedTimes = "at " ++ intercalate ", " (map fromJust formattedTimes)
  | otherwise = describedMinute ++ ", " ++ description (ListField ls) hourDescriptor
  where formattedTimes = map formatBaseField (bf : bfs)
        formatBaseField (SpecificField' s) = Just $ formatTime minuteSF s
        formatBaseField f@(RangeField' _)  = Just $ describedMinute ++ " " ++ description (Field f) hourDescriptor
        formatBaseField _                  = Nothing
        describedMinute = description (Field (SpecificField' minuteSF)) minuteDescriptor

describe :: CronSchedule -> String
describe cs =
  describeTime (minute cs) (hour cs)                           ++ ", " ++
  description (dayOfMonthSpec $ dayOfMonth cs) domDescriptor   ++ ", " ++
  description (dayOfWeekSpec $ dayOfWeek cs)   dowDescriptor   ++ ", " ++
  description (monthSpec $ month cs)           monthDescriptor
