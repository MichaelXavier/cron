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

import           Data.List                   (intercalate)

import           Data.Time.Calendar          (toGregorian)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Data.Time.Clock             (UTCTime(..))
import           Data.Time.LocalTime         (TimeOfDay(..), timeToTimeOfDay)

data CronSchedule = CronSchedule { minute :: MinuteSpec,
                                   hour :: HourSpec,
                                   dayOfMonth :: DayOfMonthSpec,
                                   month :: MonthSpec,
                                   dayOfWeek :: DayOfWeekSpec}
                                   deriving (Eq)

instance Show CronSchedule where
  show cs = "CronSchedule " ++ parts
    where parts = unwords [show $ minute cs,
                           show $ hour cs,
                           show $ dayOfMonth cs,
                           show $ month cs,
                           show $ dayOfWeek cs]

data MinuteSpec = Minutes CronField
                  deriving (Eq)

instance Show MinuteSpec where
  show (Minutes cf) = show cf

data HourSpec = Hours CronField
                deriving (Eq)

instance Show HourSpec where
  show (Hours cf) = show cf

data DayOfMonthSpec = DaysOfMonth CronField
                      deriving (Eq)

instance Show DayOfMonthSpec where
  show (DaysOfMonth cf) = show cf

data MonthSpec = Months CronField
                 deriving (Eq)

instance Show MonthSpec where
  show (Months cf) = show cf

data DayOfWeekSpec = DaysOfWeek CronField
                     deriving (Eq)

instance Show DayOfWeekSpec where
  show (DaysOfWeek cf) = show cf

data CronField = Star                  |
                 SpecificField Int     |
                 RangeField Int Int    |
                 ListField [CronField] |
                 StepField CronField Int
                 deriving (Eq)

instance Show CronField where
  show Star                  = "*"
  show (SpecificField i)     = show i
  show (RangeField x y)      = show x ++ "-" ++ show y
  show (ListField xs)        = intercalate "," $ map show xs
  show (StepField f step) = show f ++ "/" ++ show step


yearly :: CronSchedule
yearly = monthly { month = Months $ SpecificField 1 }

monthly :: CronSchedule
monthly = hourly { dayOfMonth = DaysOfMonth $ SpecificField 1 }

weekly :: CronSchedule
weekly = daily { dayOfWeek = DaysOfWeek $ SpecificField 0,
                 dayOfMonth = DaysOfMonth $ SpecificField 0 }

daily :: CronSchedule
daily = hourly { hour = Hours $ SpecificField 0 }

hourly :: CronSchedule
hourly = everyMinute { minute = Minutes $ SpecificField 0 }

everyMinute :: CronSchedule
everyMinute = CronSchedule { minute     = Minutes Star,
                             hour       = Hours Star,
                             dayOfMonth = DaysOfMonth Star,
                             month      = Months Star,
                             dayOfWeek  = DaysOfWeek Star}

scheduleMatches :: CronSchedule -> UTCTime -> Bool
scheduleMatches CronSchedule { minute     = Minutes mins,
                               hour       = Hours hrs,
                               dayOfMonth = DaysOfMonth doms,
                               month      = Months months,
                               dayOfWeek  = DaysOfWeek dows }
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
matchField x _ (RangeField y y')         = x >= y && x <= y'
matchField x unit (ListField fs)         = any (matchField x unit) fs
matchField x unit (StepField f step) = elem x $ expandDivided f step unit

expandDivided :: CronField -> Int -> CronUnit -> [Int]
expandDivided Star step unit                      = fillTo 0 max' step
  where max' = maxValue unit
expandDivided (RangeField start finish) step unit = fillTo start finish' step
  where finish' = minimum [finish, maxValue unit]
expandDivided _ _ _                               = [] -- invalid

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
