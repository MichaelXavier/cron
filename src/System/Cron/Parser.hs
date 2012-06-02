{-# LANGUAGE OverloadedStrings #-}
module System.Cron.Parser (cronSchedule) where

import           System.Cron

import           Control.Applicative  (pure, (*>), (<$>), (<*), (<*>), (<|>))
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

-- | Attoparsec Parser for a cron schedule. Complies fully with the standard
-- cron format.  Also includes the following shorthand formats which cron also
-- supports: @yearly, @monthly, @weekly, @daily, @hourly
cronSchedule :: Parser CronSchedule
cronSchedule = yearlyP  <|>
               monthlyP <|>
               weeklyP  <|>
               dailyP   <|>
               hourlyP  <|>
               classicP

---- Internals
classicP :: Parser CronSchedule
classicP = CronSchedule <$> (minutesP    <* space)
                        <*> (hoursP      <* space)
                        <*> (dayOfMonthP <* space)
                        <*> (monthP      <* space)
                        <*> (dayOfWeekP  <* A.endOfInput)
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
