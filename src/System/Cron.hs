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
                    nextMatch
                   ) where


import           Data.Ix
import qualified Data.List.NonEmpty          as NE
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate
import           Data.Time.Clock
import           Data.Time.LocalTime         (TimeOfDay (..), timeToTimeOfDay)
import Debug.Trace

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
nextMatch CronSchedule {..} (UTCTime d t) =
  UTCTime d2 t2
  where
    t2 = nextMatchingTime minute hour t
    dayRollover = t2 <= t
    startDay = if dayRollover
                  then succ d
                  else d
    d2 = nextMatchingDay dayOfMonth month dayOfWeek startDay


-------------------------------------------------------------------------------
-- | Calculate the next matching time from the given time. Because of the way time loops daily, if the resulting DiffTime is > the original, you can assume its the same day, otherwise that its the next day
nextMatchingTime :: MinuteSpec -> HourSpec -> DiffTime -> DiffTime
nextMatchingTime ms hs t = fromIntegral ((nextHour * 60 * 60) + nextMin * 60)
  where
    minuteSequence = [curMin+1..59] ++ [0..59]
    (curHour, curMin) = timeOfDay t
    hourRollover = nextMin <= curMin
    startHour = if hourRollover
                   then succ curHour
                   else curHour
    nextHour =  head [hr | hr <- [startHour..23] ++ [0..23], matchField hr CHour hsf]
    nextMin = head [mn | mn <- minuteSequence, matchField mn CMinute msf]
    
    hsf = hourSpec hs
    msf = minuteSpec ms


-------------------------------------------------------------------------------
timeOfDay :: DiffTime -> (Int, Int)
timeOfDay t = (h, m)
  where
    seconds = floor t
    minutes = seconds `div` 60
    (h, m) = minutes `divMod` 60


-------------------------------------------------------------------------------
-- | Finds the next matching day for the given specs. Choosing the
-- same day *is* allowed, so let the result of 'nextMatchingTime'
-- determine which day to start on (i.e. if it rolled over, we start tomorrow).
nextMatchingDay :: DayOfMonthSpec -> MonthSpec -> DayOfWeekSpec -> Day -> Day
nextMatchingDay dms ms dows d
  -- | domMatch && monthMatch && dowMatch = d
  --TODO: our bug is that this is allowed for dow: 0-1/2
  | domMatch && monthMatch && (dowMatch || traceShow ("curDow", curDow, dows) False) = d
  | otherwise                          = nextMatchingDay dms ms dows (succ d)
  where
    domMatch = if matchField curDom CDayOfMonth (dayOfMonthSpec dms)
                 then trace "dom match" True
                 else trace "no dom match" False
    monthMatch = if matchField curMonth CMonth (monthSpec ms)
                   then trace "month match" True
                   else trace "no month match" False
    dowMatch = if matchField curDow CDayOfWeek (dayOfWeekSpec dows)
                 then trace "dow match" True
                 else trace "no dow match" False
    (_, curMonth, curDom) = toGregorian d
    (_, _, curDow) = toWeekDate d


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
expandDivided Star step unit = fillTo 0 max' step
  where max' = maxValue unit
expandDivided (RangeField' rf) step unit = fillTo start finish' step
  where
    finish' = minimum [finish, maxValue unit]
    start = rfBegin rf
    finish = rfEnd rf
expandDivided (SpecificField' sf) step unit = dropWhile (< startAt) (expandDivided Star step unit)
  where
    startAt = specificField sf

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
