{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Cron.Types
    ( CronSchedule(..),
      Crontab(..),
      CrontabEntry(..),
      MinuteSpec,
      minuteSpec,
      mkMinuteSpec,
      HourSpec,
      hourSpec,
      mkHourSpec,
      MonthSpec,
      monthSpec,
      mkMonthSpec,
      DayOfMonthSpec,
      dayOfMonthSpec,
      mkDayOfMonthSpec,
      DayOfWeekSpec,
      dayOfWeekSpec,
      mkDayOfWeekSpec,
      BaseField(..),
      CronField(..)
    ) where


import qualified Data.Foldable      as FT
import           Data.Ix
import           Data.List          (intercalate)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Text          (Text, unpack)


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
newtype Crontab = Crontab { crontabEntries :: [CrontabEntry] }
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
newtype MinuteSpec = Minutes { minuteSpec :: CronField }
                   deriving (Eq)

instance Show MinuteSpec where
  show (Minutes cf) = show cf

--TODO: qc all of these
mkMinuteSpec :: CronField -> Maybe MinuteSpec
mkMinuteSpec cf
  | validCF cf 0 59 = Just (Minutes cf)
  | otherwise       = Nothing


-- | Hours field of a cron expression
newtype HourSpec = Hours { hourSpec :: CronField }
                 deriving (Eq)

instance Show HourSpec where
  show (Hours cf) = show cf

mkHourSpec :: CronField -> Maybe HourSpec
mkHourSpec cf
  | validCF cf 0 23 = Just (Hours cf)
  | otherwise       = Nothing


-- | Day of month field of a cron expression
newtype DayOfMonthSpec = DaysOfMonth { dayOfMonthSpec :: CronField }
                       deriving (Eq)

instance Show DayOfMonthSpec where
  show (DaysOfMonth cf) = show cf

mkDayOfMonthSpec :: CronField -> Maybe DayOfMonthSpec
mkDayOfMonthSpec cf
  | validCF cf 1 31 = Just (DaysOfMonth cf)
  | otherwise       = Nothing

-- | Month field of a cron expression
newtype MonthSpec = Months { monthSpec :: CronField }
                  deriving (Eq)

instance Show MonthSpec where
  show (Months cf) = show cf

mkMonthSpec :: CronField -> Maybe MonthSpec
mkMonthSpec cf
  | validCF cf 1 12 = Just (Months cf)
  | otherwise       = Nothing

-- | Day of week field of a cron expression
newtype DayOfWeekSpec = DaysOfWeek { dayOfWeekSpec :: CronField }
                      deriving (Eq)

instance Show DayOfWeekSpec where
  show (DaysOfWeek cf) = show cf

mkDayOfWeekSpec :: CronField -> Maybe DayOfWeekSpec
mkDayOfWeekSpec cf
  -- 0-7 is a matter of some debate but we'll be liberal here
  | validCF cf 0 7  = Just (DaysOfWeek cf)
  | otherwise       = Nothing

validCF
    :: CronField
    -> Int
    -- ^ Min value
    -> Int
    -- ^ Max value
    -> Bool
validCF (Field bf) mn mx          = validBF bf mn mx
validCF (ListField bfs) mn mx     = FT.all (\bf -> validBF bf mn mx) bfs
validCF (StepField bf step) mn mx = validBF bf mn mx && inRange (mn, mx) step

validBF
    :: BaseField
    -> Int
    -- ^ Min value
    -> Int
    -- ^ Max value
    -> Bool
validBF Star _ _                 = True
validBF (SpecificField n) mn mx  = inRange (mn, mx) n
validBF (RangeField n1 n2) mn mx = inRange (mn, mx) n1 && inRange (mn, mx) n2


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
