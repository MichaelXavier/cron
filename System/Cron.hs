--TODO: rename divided to step or stepped
module System.Cron (CronSchedule(..),
                    MinuteSpec(..),
                    HourSpec(..),
                    MonthSpec(..),
                    DayOfMonthSpec(..),
                    DayOfWeekSpec(..),
                    CronField(..),
                    yearly,
                    monthly,
                    daily,
                    weekly,
                    hourly,
                    everyMinute,
                    scheduleMatches) where

import Data.Time.Calendar (toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime (timeToTimeOfDay,
                            TimeOfDay(..))

data CronSchedule = CronSchedule { minutes :: MinuteSpec,
                                   hours :: HourSpec,
                                   dayOfMonth :: DayOfMonthSpec,
                                   month :: MonthSpec,
                                   dayOfWeek :: DayOfWeekSpec}
                                   deriving (Show, Eq)

data MinuteSpec = Minutes CronField
                  deriving (Show, Eq)

data HourSpec = Hours CronField
                deriving (Show, Eq)

data DayOfMonthSpec = DaysOfMonth CronField
                      deriving (Show, Eq)

data MonthSpec = Months CronField
                 deriving (Show, Eq)

data DayOfWeekSpec = DaysOfWeek CronField
                     deriving (Show, Eq)

data CronField = Star                  |
                 SpecificField Int     |
                 Range Int Int         |
                 ListField [CronField] |
                 DividedField CronField Int
                 deriving (Show, Eq)


yearly :: CronSchedule
yearly = monthly { month = Months $ SpecificField 1 }

monthly :: CronSchedule
monthly = hourly { dayOfMonth = DaysOfMonth $ SpecificField 1 }

weekly :: CronSchedule
weekly = daily { dayOfWeek = DaysOfWeek $ SpecificField 0,
                 dayOfMonth = DaysOfMonth $ SpecificField 0 }

daily :: CronSchedule
daily = hourly { hours = Hours $ SpecificField 0 }

hourly :: CronSchedule
hourly = everyMinute { minutes = Minutes $ SpecificField 0 }

everyMinute :: CronSchedule
everyMinute = CronSchedule { minutes    = Minutes Star,
                             hours      = Hours Star,
                             dayOfMonth = DaysOfMonth Star,
                             month      = Months Star,
                             dayOfWeek  = DaysOfWeek Star}

scheduleMatches :: CronSchedule -> UTCTime -> Bool
scheduleMatches CronSchedule { minutes = Minutes mins,
                               hours   = Hours hrs,
                               dayOfMonth = DaysOfMonth doms,
                               month   = Months months,
                               dayOfWeek = DaysOfWeek dows }
                UTCTime { utctDay = uDay,
                          utctDayTime = uTime } = all id validations
  where (_, mth, dom) = toGregorian uDay
        (_, _, dow)     = toWeekDate uDay
        TimeOfDay { todHour = hr,
                    todMin  = mn} = timeToTimeOfDay uTime
        validations = map validate [(mn, CMinute, mins),
                                    (hr, CHour, hrs),
                                    (dom, CDayOfMonth, doms),
                                    (mth, CMonth, months),
                                    (dow, CDayOfWeek, dows)]
        validate (x, y, z) = matchField x y z
        
matchField :: Int -> CronUnit -> CronField -> Bool
matchField _ _ Star                      = True
matchField x _ (SpecificField y)         = x == y
matchField x _ (Range y y')              = x >= y && x <= y'
matchField x unit (ListField fs)         = any (matchField x unit) fs
matchField x unit (DividedField f step) = elem x $ expandDivided f step unit

expandDivided :: CronField -> Int -> CronUnit -> [Int]
expandDivided Star step unit                 = fillTo 0 max' step
  where max' = maxValue unit
expandDivided (Range start finish) step unit = fillTo start finish' step
  where finish' = minimum [finish, maxValue unit] 
expandDivided _ _ _                          = [] -- invalid

fillTo :: Int -> Int -> Int -> [Int]
fillTo start finish step
  | step <= 0      = []
  | finish < start = []
  | otherwise      = [ x | x <- [start..finish], x `mod` step == 0]

data CronUnit = CMinute     |
                CHour       |
                CDayOfMonth |
                CMonth      |
                CDayOfWeek

maxValue :: CronUnit -> Int
maxValue CMinute     = 59
maxValue CHour       = 23
maxValue CDayOfMonth = 31
maxValue CMonth      = 12
maxValue CDayOfWeek  = 6
        

--toDiffTimes :: CronSchedule -> [NominalDiffTime]
--toDiffTimes = undefined

--inRange :: (Bounded a, Ord a) => a -> Bool
--inRange a =  a >= minBound a && a <= maxBound a
