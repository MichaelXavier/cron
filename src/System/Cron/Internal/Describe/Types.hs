{-# LANGUAGE RecordWildCards #-}

module System.Cron.Internal.Describe.Types where

import Data.List  (intercalate)
import Data.Maybe (catMaybes)
import Data.Time (TimeLocale, TimeZone)


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


data Month = January | February | March     | April   | May      | June     |
             July    | August   | September | October | November | December
             deriving (Enum, Bounded, Show)


safeIntToMonth :: Int -> Month
safeIntToMonth = toEnum . subtract 1 . min 12 . max 1


data Weekday = Sunday | Monday | Tuesday | Wednesday |
               Thursday | Friday | Saturday | Sunday2
               deriving (Enum, Bounded, Show)


safeIntToWeekDay :: Int -> Weekday
safeIntToWeekDay n
  | n == 7    = Sunday
  | otherwise = toEnum . min 6 $ max 0 n


data Verbosity = Verbose | NotVerbose


data TimeFormat = Hour24 | Hour12 | CustomTimeFormat TimeZone TimeLocale String


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
