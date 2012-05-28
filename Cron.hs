{-# LANGUAGE FlexibleInstances #-}
module Cron where

import Control.Applicative ((<*>),
                            (*>),
                            (<$>),
                            (<|>),
                            pure)

import Data.Ix (range)
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC
import Data.Attoparsec.Text (Parser)
import Data.Text (Text)
import Data.Time

--TODO: peek first character
cronSchedule :: Parser CronSchedule
cronSchedule = yearlyP  <|>
               monthlyP <|>
               weeklyP  <|>
               dailyP   <|>
               hourlyP  <|>
               classicP

data CronSchedule = CronSchedule { minutes :: MinuteSpec,
                                   hours :: HourSpec,
                                   dayOfMonth :: DayOfMonthSpec,
                                   month :: MonthSpec,
                                   dayOfWeek :: DayOfWeekSpec}

data MinuteSpec = Minutes [Int] | EveryMinute

data HourSpec = Hours [Int] | EveryHour

data DayOfMonthSpec = DaysOfMonth [Int] | EveryDayOfMonth

data MonthSpec = Months [Int] | EveryMonth

data DayOfWeekSpec = DaysOfWeek [Int] | EveryDayOfWeek

data CronUnit = CMinute | CHour | CDayOfMonth | CMonth | CDayOfWeek

instance Bounded CronUnit where
  minBound CMonth      = 1
  minBound CDayOfMonth = 1
  minBound _           = 0
  maxBound CMinute     = 59
  maxBound CHour       = 23
  maxBound CDayOfMonth = 31
  maxBound CMonth      = 12
  maxBound CDayOfWeek  = 6

--class HasCronUnit a where
--  toCronUnit :: a -> CronUnit

-- parser internals

yearlyP :: Parser CronSchedule
yearlyP  = A.string "@yearly"  *> pure yearly

monthlyP :: Parser CronSchedule
monthlyP = A.string "@monthly" *> pure monthly

weeklyP :: Parser CronSchedule
weeklyP  = A.string "@weekly"  *> pure weekly

dailyP :: Parser CronSchedule
dailyP   = A.string "@daily"   *> pure daily

hourlyP :: Parser CronSchedule
hourlyP  = A.string "@hourly"  *> pure hourly

classicP :: Parser CronSchedule
classicP = CronSchedule <$> minutesP
                        <*> hoursP
                        <*> dayOfMonthP
                        <*> monthP
                        <*> dayOfWeekP

--TODO: must handle a combination of many of these. EITHER just *, OR a list of 
minutesP :: Parser MinuteSpec
minutesP = parseStar EveryMinute <|> parseList Minutes

hoursP :: Parser HourSpec
hoursP = parseStar EveryHour <|> parseList Hours

dayOfMonthP :: Parser DayOfMonthSpec
dayOfMonthP = parseStar EveryDayOfMonth <|> parseList DaysOfMonth

monthP :: Parser MonthSpec
monthP = parseStar EveryMonth <|> parseList Months

dayOfWeekP :: Parser DayOfWeekSpec
dayOfWeekP = parseStar EveryDayOfWeek <|> parseList DaysOfWeek

parseStar :: Parser a
parseStar constr = A.char '*' *> constr

parseList :: SpecParser a
parseList constr = parseDivisor constr <|>
                   parseRange constr   <|>
                   parseSingleValue constr

parseDivisor :: SpecParser a
parseDivisor constr = undefined

parseRange :: SpecParser a
parseRange constr = do start <- parseInt
                       A.char '-'
                       end   <- parseInt
                       if start <= end
                        then toCronSpec constr (start, end)
                        else rangeInvalid
  where rangeInvalid = fail "start of range must be less than or equal to end"

parseSingleValue :: SpecParser a
parseSingleValue constr = toCronSpec constr <$> parseInt

type SpecConstr a = [Int] -> a
type SpecParser a = SpecConstr a -> Parser a

parseInt :: Parser Int
parseInt = fromIntegral <$> A.decimal

class ToCronSpec a where
  toCronSpec :: a -> SpecConstr b -> b

instance ToCronSpec Int where
  toCronSpec constr i = constr [i]

instance ToCronSpec (Int, Int) where
  toCronSpec constr = constr . range

yearly :: CronSchedule
yearly = monthly { month = Months [1] }

monthly :: CronSchedule
monthly = hourly { dayOfMonth = DaysOfMonth [1] }

weekly :: CronSchedule
weekly = daily { dayOfWeek = DaysOfWeek [0],
                 dayOfMonth = DaysOfMonth [0] }

daily :: CronSchedule
daily = hourly { hours = Hours [0] }

hourly :: CronSchedule
hourly = everyMinute { minutes = Minutes [0] }

everyMinute :: CronSchedule
everyMinute = CronSchedule { minutes    = EveryMinute,
                             hours      = EveryHour,
                             dayOfMonth = EveryDayOfMonth,
                             month      = EveryMonth,
                             dayOfWeek  = EveryDayOfWeek}

--toDiffTimes :: CronSchedule -> [NominalDiffTime]
--toDiffTimes = undefined

inRange :: (Bounded a, Ord a) => a -> Bool
inRange a =  a >= minBound a && a <= maxBound a
