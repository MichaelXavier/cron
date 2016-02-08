{-# LANGUAGE RecordWildCards #-}
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
module System.Cron (module System.Cron.Types,
                    yearly,
                    monthly,
                    daily,
                    weekly,
                    hourly,
                    everyMinute,
                    scheduleMatches,
                    nextMatch) where


import           Data.Ix
import qualified Data.List.NonEmpty          as NE
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate
import           Data.Time.Clock
import           Data.Time.LocalTime         (TimeOfDay (..), timeToTimeOfDay)

import           System.Cron.Types




-- | Shorthand for every January 1st at midnight. Parsed with \@yearly, 0 0 1 1 *
yearly :: CronSchedule
yearly = monthly { month = fromJust . mkMonthSpec . Field . SpecificField' . fromJust . mkSpecificField $ 1 }

-- | Shorthand for every 1st of the month at midnight. Parsed with \@monthly, 0 0 1 * *
monthly :: CronSchedule
monthly = daily { dayOfMonth = fromJust . mkDayOfMonthSpec . Field . SpecificField' . fromJust . mkSpecificField $ 1 }

-- | Shorthand for every sunday at midnight. Parsed with \@weekly, 0 0 * * 0
weekly :: CronSchedule
weekly = daily { dayOfWeek = fromJust . mkDayOfWeekSpec . Field . SpecificField' . fromJust . mkSpecificField $ 0 }

-- | Shorthand for every day at midnight. Parsed with \@daily, 0 0 * * *
daily :: CronSchedule
daily = hourly { hour = fromJust . mkHourSpec . Field . SpecificField' . fromJust . mkSpecificField $ 0 }

-- | Shorthand for every hour on the hour. Parsed with \@hourly, 0 * * * *
hourly :: CronSchedule
hourly = everyMinute { minute = fromJust . mkMinuteSpec . Field . SpecificField' . fromJust . mkSpecificField $ 0 }

-- | Shorthand for an expression that always matches. Parsed with * * * * *
everyMinute :: CronSchedule
everyMinute = CronSchedule { minute     = fromJust . mkMinuteSpec . Field $ Star,
                             hour       = fromJust . mkHourSpec . Field $ Star,
                             dayOfMonth = fromJust . mkDayOfMonthSpec . Field $ Star,
                             month      = fromJust . mkMonthSpec . Field $ Star,
                             dayOfWeek  = fromJust . mkDayOfWeekSpec . Field $ Star}

-- | Determines if the given time is matched by the given schedule. A
-- periodical task would use this to determine if an action needs to be
-- performed at the current time or not.
scheduleMatches :: CronSchedule
                   -> UTCTime
                   -> Bool
scheduleMatches CronSchedule {..}
                UTCTime { utctDay = uDay,
                          utctDayTime = uTime } = if restricted doms && restricted dows
                                                  then mnv && hrv && mthv && (domv || dowv)
                                                  else mnv && hrv && mthv && domv && dowv
  where mins = minuteSpec minute
        hrs = hourSpec hour
        doms = dayOfMonthSpec dayOfMonth
        months = monthSpec month
        dows = dayOfWeekSpec dayOfWeek
        (_, mth, dom) = toGregorian uDay
        (_, _, dow) = toWeekDate uDay
        TimeOfDay { todHour = hr,
                    todMin  = mn} = timeToTimeOfDay uTime
        [mnv,hrv,domv,mthv,dowv] = map validate [(mn, CMinute, mins),
                                                 (hr, CHour, hrs),
                                                 (dom, CDayOfMonth, doms),
                                                 (mth, CMonth, months),
                                                 (dow, CDayOfWeek, dows)]
        validate (x, y, z) = matchField x y z
        restricted (Field Star)               = False
        restricted (Field (SpecificField' _)) = True
        restricted (Field (RangeField' _))    = True
        restricted (ListField _)              = True
        restricted (StepField' sf)            = restricted (Field (sfField sf))

nextMatch :: CronSchedule -> UTCTime -> UTCTime
nextMatch cs t = head (filter (scheduleMatches cs) (take 1000 $ nextMinutes t)) --TODO: drop take. could actually use a take of 1 year
    --FIXME: slowest and dumbest possible way


nextMinutes :: UTCTime -> [UTCTime]
nextMinutes t = [ addMinutes tRounded mins | mins <- [1..]]
  where
    addMinutes time mins = addUTCTime (fromInteger (60 * mins)) time
    -- round down to nearest 60
    tRounded = t { utctDayTime = roundToMinute (utctDayTime t)}



roundToMinute :: DiffTime -> DiffTime
roundToMinute n = secondsToDiffTime (nInt - (nInt `mod` 60))
  where
    nInt = round n


--TODO: break out basic field into another function?
matchField :: Int
              -> CronUnit
              -> CronField
              -> Bool
matchField x unit (Field f)       = matchField' x unit f
matchField x unit (ListField fs)  = any (matchField' x unit) . NE.toList $ fs
matchField x unit (StepField' sf) = elem x $ expandDivided f step unit
  where
    f = sfField sf
    step = sfStepping sf

matchField' :: Int
               -> CronUnit
               -> BaseField
               -> Bool
matchField' _ _ Star                        = True
matchField' x CDayOfWeek (SpecificField' sf)
  | x == y || x == 0 && y == 7 || x == 7 && y == 0 = True
  | otherwise                                      = False
  where
    y = specificField sf
matchField' x _ (SpecificField' sf) = x == specificField sf
matchField' x _ (RangeField' rf) = inRange (y, y') x
  where
    y = rfBegin rf
    y' = rfEnd rf

expandDivided :: BaseField
                 -> Int
                 -> CronUnit
                 -> [Int]
expandDivided Star step unit                      = fillTo 0 max' step
  where max' = maxValue unit
expandDivided (RangeField' rf) step unit = fillTo start finish' step
  where
    finish' = minimum [finish, maxValue unit]
    start = rfBegin rf
    finish = rfEnd rf
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
