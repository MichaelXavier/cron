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
-- > import System.Cron.Parser
-- >
-- > main :: IO ()
-- > main = don
-- >   print $ parseCronSchedule "*/2 * 3 * 4,5,6"
--
--------------------------------------------------------------------
module System.Cron.Parser
    ( -- * Parsers
      cronSchedule
    , cronScheduleLoose
    , crontab
    , crontabEntry
    -- * Convenience Functions
    , parseCronSchedule
    , parseCrontab
    , parseCrontabEntry
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative  as Ap
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import           Data.Char            (isSpace)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Text            (Text)
-------------------------------------------------------------------------------
import           System.Cron.Types
-------------------------------------------------------------------------------


-- | Attoparsec Parser for a cron schedule. Complies fully with the standard
-- cron format.  Also includes the following shorthand formats which cron also
-- supports: \@yearly, \@monthly, \@weekly, \@daily, \@hourly. Note that this
-- parser will fail if there is extraneous input. This is to prevent things
-- like extra fields. If you want a more lax parser, use 'cronScheduleLoose',
-- which is fine with extra input.
cronSchedule :: Parser CronSchedule
cronSchedule = cronScheduleLoose <* A.endOfInput


-------------------------------------------------------------------------------
-- | Same as 'cronSchedule' but does not fail on extraneous input.
cronScheduleLoose :: Parser CronSchedule
cronScheduleLoose = yearlyP  <|>
                    monthlyP <|>
                    weeklyP  <|>
                    dailyP   <|>
                    hourlyP  <|>
                    classicP


-------------------------------------------------------------------------------
-- | Parses a full crontab file, omitting comments and including environment
-- variable sets (e.g FOO=BAR).
crontab :: Parser Crontab
crontab = Crontab <$> A.sepBy lineP (A.char '\n')
  where lineP    = A.skipMany commentP *> crontabEntry
        commentP = A.skipSpace *> A.char '#' *> skipToEOL


-------------------------------------------------------------------------------
-- | Parses an individual crontab line, which is either a scheduled command or
-- an environmental variable set.
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
                                     <*> (A.skipSpace *> (CronCommand <$> takeToEOL))


-------------------------------------------------------------------------------
-- Convenience functions
-------------------------------------------------------------------------------


parseCronSchedule :: Text -> Either String CronSchedule
parseCronSchedule = A.parseOnly cronSchedule


-------------------------------------------------------------------------------
parseCrontab :: Text -> Either String Crontab
parseCrontab = A.parseOnly crontab


-------------------------------------------------------------------------------
parseCrontabEntry :: Text -> Either String CrontabEntry
parseCrontabEntry = A.parseOnly crontabEntry


-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------


takeToEOL :: Parser Text
takeToEOL = A.takeTill (== '\n') -- <* A.skip (== '\n')


-------------------------------------------------------------------------------
skipToEOL :: Parser ()
skipToEOL = A.skipWhile (/= '\n')


-------------------------------------------------------------------------------
classicP :: Parser CronSchedule
classicP = CronSchedule <$> (minutesP    <* space)
                        <*> (hoursP      <* space)
                        <*> (dayOfMonthP <* space)
                        <*> (monthP      <* space)
                        <*> dayOfWeekP
  where space = A.char ' '


-------------------------------------------------------------------------------
cronFieldP :: Parser CronField
cronFieldP = stepP  <|>
             listP  <|>
             fieldP

  where
    fieldP        = Field <$> baseFieldP
    listP         = ListField <$> neListP baseFieldP
    stepP         = StepField' <$> stepFieldP


-------------------------------------------------------------------------------
stepFieldP :: Parser StepField
stepFieldP = do
  f <- baseFieldP
  _ <- A.char '/'
  mParse (mkStepField f) "invalid stepping" =<< parseInt


-------------------------------------------------------------------------------
neListP :: Parser a -> Parser (NonEmpty a)
neListP p = coerceNE =<< A.sepBy1 p (A.char ',')
  where
    coerceNE []     = fail "expected non-empty list"
    coerceNE [_]    = fail "invalid singleton list"
    coerceNE (x:xs) = return $ x :| xs


-------------------------------------------------------------------------------
baseFieldP :: Parser BaseField
baseFieldP = rangeP <|>
             starP  <|>
             specificP
  where starP         = A.char '*' *> Ap.pure Star
        rangeP        = RangeField' <$> rangeFieldP
        specificP     = SpecificField' <$> specificFieldP


-------------------------------------------------------------------------------
specificFieldP :: Parser SpecificField
specificFieldP =
  mParse mkSpecificField "specific field value out of range" =<< parseInt


-------------------------------------------------------------------------------
rangeFieldP :: Parser RangeField
rangeFieldP = do
  begin <- parseInt


  _ <- A.char '-'
  end <- parseInt
  mParse (mkRangeField begin) "start of range must be less than or equal to end" end


-------------------------------------------------------------------------------
yearlyP :: Parser CronSchedule
yearlyP  = A.string "@yearly"  *> pure yearly


-------------------------------------------------------------------------------
monthlyP :: Parser CronSchedule
monthlyP = A.string "@monthly" *> pure monthly


-------------------------------------------------------------------------------
weeklyP :: Parser CronSchedule
weeklyP  = A.string "@weekly"  *> pure weekly


-------------------------------------------------------------------------------
dailyP :: Parser CronSchedule
dailyP   = A.string "@daily"   *> pure daily


-------------------------------------------------------------------------------
hourlyP :: Parser CronSchedule
hourlyP  = A.string "@hourly"  *> pure hourly


-------------------------------------------------------------------------------
minutesP :: Parser MinuteSpec
minutesP = mParse mkMinuteSpec "minutes out of range" =<< cronFieldP


-------------------------------------------------------------------------------
hoursP :: Parser HourSpec
hoursP = mParse mkHourSpec "hours out of range" =<< cronFieldP


-------------------------------------------------------------------------------
dayOfMonthP :: Parser DayOfMonthSpec
dayOfMonthP = mParse mkDayOfMonthSpec "day of month out of range" =<< cronFieldP


-------------------------------------------------------------------------------
monthP :: Parser MonthSpec
monthP = mParse mkMonthSpec "month out of range" =<< cronFieldP


-------------------------------------------------------------------------------
dayOfWeekP :: Parser DayOfWeekSpec
dayOfWeekP = mParse mkDayOfWeekSpec "day of week out of range" =<< cronFieldP


-------------------------------------------------------------------------------
parseInt :: Parser Int
parseInt = A.decimal


-------------------------------------------------------------------------------
mParse :: (Monad m) => (a -> Maybe b) -> String -> a -> m b
mParse f msg = maybe (fail msg) return . f
