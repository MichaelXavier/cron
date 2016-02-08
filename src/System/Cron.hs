--------------------------------------------------------------------
-- |
-- Module      : System.Cron
-- Description : Datatype for Cron Schedule and helpful functions
-- Copyright   : (c) Michael Xavier 2012
-- License     : MIT
--
-- Maintainer: Michael Xavier <michael@michaelxavier.net>
-- Portability: portable
--
-- Toplevel module for Cron specifying a cron schedule and several convenience
-- functions for dealing with cron schedules
--
-- > import Control.Concurrent
-- > import Control.Monad
-- > import Data.Time.Clock
-- > import System.Cron
-- >
-- > main :: IO ()
-- > main = forever $ do
-- >          now <- getCurrentTime
-- >          when (scheduleMatches schedule now) doWork
-- >          putStrLn "sleeping"
-- >          threadDelay 100000
-- >        where doWork   = putStrLn "Time to work"
-- >              schedule = hourly
--
--------------------------------------------------------------------
module System.Cron (CronSchedule(..),
                    Crontab(..),
                    CrontabEntry(..),
                    MinuteSpec(..),
                    HourSpec(..),
                    MonthSpec(..),
                    DayOfMonthSpec(..),
                    DayOfWeekSpec(..),
                    BaseField(..),
                    CronField(..),
                    yearly,
                    monthly,
                    daily,
                    weekly,
                    hourly,
                    everyMinute,
                    scheduleMatches,
                    nextMatch) where

import           Data.List (intercalate)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import           Data.Text                   (Text, unpack)
import           Data.Time.Calendar          (toGregorian)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Data.Time.Clock             (UTCTime(..))
import           Data.Time.LocalTime         (TimeOfDay(..), timeToTimeOfDay)

-- | Specification for a cron expression
data CronSchedule = CronSchedule { minute     :: MinuteSpec,     -- ^ Which minutes to run. First field in a cron specification.
                                   hour       :: HourSpec,       -- ^ Which hours to run. Second field in a cron specification.
                                   dayOfMonth :: DayOfMonthSpec, -- ^ Which days of the month to run. Third field in a cron specification.
                                   month      :: MonthSpec,      -- ^ Which months to run. Fourth field in a cron specification.
                                   dayOfWeek  :: DayOfWeekSpec   -- ^ Which days of the week to run. Fifth field in a cron specification.
                                 }
                                   deriving (Eq)

instance Show CronSchedule where
  show cs = "CronSchedule " ++ showRaw cs

showRaw :: CronSchedule
           -> String
showRaw cs = unwords [show $ minute cs,
                      show $ hour cs,
                      show $ dayOfMonth cs,
                      show $ month cs,
                      show $ dayOfWeek cs]

-- | Crontab file, omitting comments.
newtype Crontab = Crontab [CrontabEntry]
                  deriving (Eq)

instance Show Crontab where
  show (Crontab entries) = intercalate "\n" . map show $ entries

-- | Essentially a line in a crontab file. It is either a schedule with a
-- command after it or setting an environment variable (e.g. FOO=BAR)
data CrontabEntry = CommandEntry { schedule :: CronSchedule,
                                   command  :: Text} |
                    EnvVariable  { varName  :: Text,
                                   varValue :: Text }
                    deriving (Eq)

instance Show CrontabEntry where
  show CommandEntry { schedule = s, command = c} = showRaw s ++ " " ++ unpack c
  show EnvVariable  { varName = n, varValue = v} = unpack n ++ "=" ++ unpack v

-- | Minutes field of a cron expression
data MinuteSpec = Minutes CronField
                  deriving (Eq)

instance Show MinuteSpec where
  show (Minutes cf) = show cf

-- | Hours field of a cron expression
data HourSpec = Hours CronField
                deriving (Eq)

instance Show HourSpec where
  show (Hours cf) = show cf

-- | Day of month field of a cron expression
data DayOfMonthSpec = DaysOfMonth CronField
                      deriving (Eq)

instance Show DayOfMonthSpec where
  show (DaysOfMonth cf) = show cf

-- | Month field of a cron expression
data MonthSpec = Months CronField
                 deriving (Eq)

instance Show MonthSpec where
  show (Months cf) = show cf

-- | Day of week field of a cron expression
data DayOfWeekSpec = DaysOfWeek CronField
                     deriving (Eq)

instance Show DayOfWeekSpec where
  show (DaysOfWeek cf) = show cf

-- | Individual field of a cron expression.
data BaseField = Star              | -- ^ Matches anything
                 SpecificField Int | -- ^ Matches a specific value (e.g. 1)
                 RangeField Int Int  -- ^ Matches a range of values (e.g. 1-3)
               deriving (Eq)

instance Show BaseField where
  show Star                  = "*"
  show (SpecificField i)     = show i
  show (RangeField x y)      = show x ++ "-" ++ show y

data CronField = Field BaseField                |
                 ListField (NonEmpty BaseField) | -- ^ Matches a list of expressions.
                 StepField BaseField Int          -- ^ Matches a stepped expression, e.g. (*/2).
                 deriving (Eq)

instance Show CronField where
  show (Field f)             = show f
  show (ListField xs)        = intercalate "," . NE.toList . NE.map show $ xs
  show (StepField f step) = show f ++ "/" ++ show step


-- | Shorthand for every January 1st at midnight. Parsed with \@yearly, 0 0 1 1 *
yearly :: CronSchedule
yearly = monthly { month = Months . Field . SpecificField $ 1 }

-- | Shorthand for every 1st of the month at midnight. Parsed with \@monthly, 0 0 1 * *
monthly :: CronSchedule
monthly = daily { dayOfMonth = DaysOfMonth . Field . SpecificField $ 1 }

-- | Shorthand for every sunday at midnight. Parsed with \@weekly, 0 0 * * 0
weekly :: CronSchedule
weekly = daily { dayOfWeek = DaysOfWeek . Field . SpecificField $ 0 }

-- | Shorthand for every day at midnight. Parsed with \@daily, 0 0 * * *
daily :: CronSchedule
daily = hourly { hour = Hours . Field . SpecificField $ 0 }

-- | Shorthand for every hour on the hour. Parsed with \@hourly, 0 * * * *
hourly :: CronSchedule
hourly = everyMinute { minute = Minutes . Field . SpecificField $ 0 }

-- | Shorthand for an expression that always matches. Parsed with * * * * *
everyMinute :: CronSchedule
everyMinute = CronSchedule { minute     = Minutes . Field $ Star,
                             hour       = Hours . Field $ Star,
                             dayOfMonth = DaysOfMonth . Field $ Star,
                             month      = Months . Field $ Star,
                             dayOfWeek  = DaysOfWeek . Field $ Star}

-- | Determines if the given time is matched by the given schedule. A
-- periodical task would use this to determine if an action needs to be
-- performed at the current time or not.
scheduleMatches :: CronSchedule
                   -> UTCTime
                   -> Bool
scheduleMatches CronSchedule { minute     = Minutes mins,
                               hour       = Hours hrs,
                               dayOfMonth = DaysOfMonth doms,
                               month      = Months months,
                               dayOfWeek  = DaysOfWeek dows }
                UTCTime { utctDay = uDay,
                          utctDayTime = uTime } = if restricted doms && restricted dows
                                                  then mnv && hrv && mthv && (domv || dowv)
                                                  else mnv && hrv && mthv && domv && dowv
  where (_, mth, dom) = toGregorian uDay
        (_, _, dow) = toWeekDate uDay
        TimeOfDay { todHour = hr,
                    todMin  = mn} = timeToTimeOfDay uTime
        [mnv,hrv,domv,mthv,dowv] = map validate [(mn, CMinute, mins),
                                                 (hr, CHour, hrs),
                                                 (dom, CDayOfMonth, doms),
                                                 (mth, CMonth, months),
                                                 (dow, CDayOfWeek, dows)]
        validate (x, y, z) = matchField x y z
        --TODO: rename
        restricted (Field Star)              = False
        restricted (Field (SpecificField _)) = True
        restricted (Field (RangeField _ _))  = True
        restricted (ListField _)             = True
        restricted (StepField f _)           = restricted (Field f)

nextMatch :: CronSchedule -> UTCTime -> Maybe UTCTime
nextMatch = undefined

--TODO: break out basic field into another function?
matchField :: Int
              -> CronUnit
              -> CronField
              -> Bool
matchField x unit (Field f)          = matchField' x unit f
matchField x unit (ListField fs)     = any (matchField' x unit) . NE.toList $ fs
matchField x unit (StepField f step) = elem x $ expandDivided f step unit

matchField' :: Int
               -> CronUnit
               -> BaseField
               -> Bool
matchField' _ _ Star                        = True
matchField' x CDayOfWeek (SpecificField y)
  | x == y || x == 0 && y == 7 || x == 7 && y == 0 = True
  | otherwise                                      = False
matchField' x _ (SpecificField y)         = x == y
matchField' x _ (RangeField y y')         = x >= y && x <= y'

expandDivided :: BaseField
                 -> Int
                 -> CronUnit
                 -> [Int]
expandDivided Star step unit                      = fillTo 0 max' step
  where max' = maxValue unit
expandDivided (RangeField start finish) step unit = fillTo start finish' step
  where finish' = minimum [finish, maxValue unit]
expandDivided _ _ _                               = [] -- invalid

fillTo :: Int
          -> Int
          -> Int
          -> [Int]
fillTo start finish step
  | step <= 0      = []
  | finish < start = []
  | otherwise      = takeWhile (<= finish) nums
  where nums = map (start +) adds
        adds = map (*2) [0..]

data CronUnit = CMinute     |
                CHour       |
                CDayOfMonth |
                CMonth      |
                CDayOfWeek deriving (Show, Eq)

maxValue :: CronUnit -> Int
maxValue CMinute     = 59
maxValue CHour       = 23
maxValue CDayOfMonth = 31
maxValue CMonth      = 12
maxValue CDayOfWeek  = 6
