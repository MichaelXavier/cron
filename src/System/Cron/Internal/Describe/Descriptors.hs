module System.Cron.Internal.Describe.Descriptors where

import System.Cron.Internal.Describe.Time
import System.Cron.Internal.Describe.Types

minuteDescriptor :: Descriptor
minuteDescriptor = Descriptor {
    pluralDesc = "minutes"
  , singularDesc = "minute"
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
  where
    sss n
      | n == 0    = Nothing
      | otherwise = Just $ "starting at " ++ show n ++ " minutes past the hour"


hourDescriptor :: TimeFormat -> Descriptor
hourDescriptor tf = Descriptor {
    pluralDesc = "hours"
  , singularDesc = "hour"
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
  where toHour h = format tf (Minute 0) (Hour h)
        sss n
          | n == 0    = Nothing
          | otherwise = Just $ "starting at " ++ toHour n


domDescriptor :: Descriptor
domDescriptor = Descriptor {
    pluralDesc = "days"
  , singularDesc = "day"
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
    pluralDesc = "months"
  , singularDesc = "month"
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
        sss n
          | n == 1    = Nothing
          | otherwise = Just $ toMonth n ++ " through " ++ toMonth 12


dowDescriptor :: Descriptor
dowDescriptor = Descriptor {
    pluralDesc = "days of the week"
  , singularDesc = "day of the week"
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
        sss n = Just $ toWeekday n ++ " through " ++ show Saturday
