{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module      : System.Cron.Parser
-- Description : Attoparsec parser for cron formatted intervals
-- Copyright   : (c) Michael Xavier 2012
-- License     : MIT
--
-- Maintainer: Michael Xavier <michael@michaelxavier.net>
-- Portability: portable
--
-- Attoparsec parser combinator for cron schedules. See cron documentation for
-- how those are formatted.
-- 
-- > import Data.Attoparsec.Text (parseOnly)
-- > import System.Cron.Parser
-- > 
-- > main :: IO ()
-- > main = do
-- >   print $ parseOnly cronSchedule "*/2 * 3 * 4,5,6"
-- 
--------------------------------------------------------------------
module System.Cron.Parser (cronSchedule,
                           cronScheduleLoose,
                           crontab,
                           crontabEntry) where

import           System.Cron

import           Control.Applicative  (pure, (*>), (<$>), (<*), (<*>), (<|>))
import           Data.Char (isSpace)
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import           Data.Text (Text)

-- | Attoparsec Parser for a cron schedule. Complies fully with the standard
-- cron format.  Also includes the following shorthand formats which cron also
-- supports: \@yearly, \@monthly, \@weekly, \@daily, \@hourly
cronSchedule :: Parser CronSchedule
cronSchedule = cronScheduleLoose <* A.endOfInput

cronScheduleLoose :: Parser CronSchedule
cronScheduleLoose = yearlyP  <|>
                    monthlyP <|>
                    weeklyP  <|>
                    dailyP   <|>
                    hourlyP  <|>
                    classicP

crontab :: Parser Crontab
crontab = Crontab <$> A.sepBy lineP (A.char '\n')
  where lineP    = A.skipMany commentP *> crontabEntry
        commentP = A.skipSpace *> A.char '#' *> skipToEOL

crontabEntry :: Parser CrontabEntry
crontabEntry = A.skipSpace *> parser
  where parser = envVariableP <|>
                 commandEntryP
        envVariableP = do var <- A.takeWhile1 (A.notInClass " =")
                          A.skipSpace
                          _   <- A.char '='
                          A.skipSpace
                          val <- A.takeWhile1 $ not . isSpace
                          A.skipWhile (\c -> c == ' ' || c == '\t')
                          return $ EnvVariable var val
        commandEntryP = CommandEntry <$> cronScheduleLoose
                                     <*> (A.skipSpace *> takeToEOL)

---- Internals
takeToEOL :: Parser Text
takeToEOL = A.takeTill (== '\n') -- <* A.skip (== '\n')

skipToEOL :: Parser ()
skipToEOL = A.skipWhile (/= '\n')

classicP :: Parser CronSchedule
classicP = CronSchedule <$> (minutesP    <* space)
                        <*> (hoursP      <* space)
                        <*> (dayOfMonthP <* space)
                        <*> (monthP      <* space)
                        <*> dayOfWeekP
  where space = A.char ' '

cronFieldP :: Parser CronField
cronFieldP = steppedP  <|>
             rangeP    <|>
             listP     <|>
             starP     <|>
             specificP
  where starP         = A.char '*' *> pure Star
        rangeP        = do start <- parseInt
                           _     <- A.char '-'
                           end   <- parseInt
                           if start <= end
                             then return $ RangeField start end
                             else rangeInvalid
        rangeInvalid  = fail "start of range must be less than or equal to end"
        -- Must avoid infinitely recursive parsers
        listP         = reduceList <$> A.sepBy1 listableP (A.char ',')
        listableP     = starP    <|>
                        rangeP   <|>
                        steppedP <|>
                        specificP
        stepListP     = ListField <$> A.sepBy1 stepListableP (A.char ',')
        stepListableP = starP  <|>
                        rangeP
        steppedP      = StepField <$> steppableP <*> (A.char '/' *> parseInt)
        steppableP    = starP     <|>
                        rangeP    <|>
                        stepListP <|>
                        specificP
        specificP     = SpecificField <$> parseInt

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


--TODO: must handle a combination of many of these. EITHER just *, OR a list of
minutesP :: Parser MinuteSpec
minutesP = Minutes <$> cronFieldP

hoursP :: Parser HourSpec
hoursP = Hours <$> cronFieldP

dayOfMonthP :: Parser DayOfMonthSpec
dayOfMonthP = DaysOfMonth <$> cronFieldP

monthP :: Parser MonthSpec
monthP = Months <$> cronFieldP

dayOfWeekP :: Parser DayOfWeekSpec
dayOfWeekP = DaysOfWeek <$> cronFieldP

parseInt :: Parser Int
parseInt = A.decimal

reduceList :: [CronField] -> CronField
reduceList []  = ListField [] -- this should not happen
reduceList [x] = x
reduceList xs  = ListField xs
