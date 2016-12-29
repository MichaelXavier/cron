{-# LANGUAGE RecordWildCards #-}

module System.Cron.Internal.DescribeTypes where

import Data.List                                (intercalate)
import Data.Maybe                               (catMaybes)
import System.Cron.Internal.DescribeUtils

data Descriptor = Descriptor {
    pluralDesc         :: String
  , singularDesc       :: String
  , rangePrefix        :: String
  , rangeSuffix        :: String
  , rangeJoiner        :: String
  , displayItem        :: Int -> String
  , specificPrefix     :: String
  , specificSuffix     :: String
  , stepSpecificSuffix :: Int -> Maybe String
  , listPrefix         :: String
  , listSuffix         :: Maybe String
  }


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


hourDescriptor :: Descriptor
hourDescriptor = Descriptor {
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
  where toHour n = leftPad n ++ ":00"
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
        -- FIXME
        sss n
          | n == 0    = Nothing
          | otherwise = Just $ toWeekday n ++ " through " ++ toWeekday 6


data Verbosity = Verbose | NotVerbose


data DescribedValue = Concrete String
                    | Every String


instance Show DescribedValue where
  show (Concrete s) = s
  show (Every s)    = s


data Time = ConcreteTime String
          | Other (Maybe DescribedValue) (Maybe DescribedValue)


instance Show Time where
  show (ConcreteTime s) = s
  show (Other md1 md2)  = intercalate ", " .
                          map show         $ catMaybes [md1, md2]


data Description = Desc {
    _time   :: Time
  , _dom    :: Maybe DescribedValue
  , _month  :: Maybe DescribedValue
  , _dow    :: Maybe DescribedValue
  }


instance Show Description where
  show Desc{..} = intercalate ", " .
                  (:) (show _time) .
                  map show         $ catMaybes [_dom, _dow, _month]
