{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Cron.Types
    ( CronSchedule(..)
    , Crontab(..)
    , CrontabEntry(..)
    , MinuteSpec
    , CronCommand(..)
    , minuteSpec
    , mkMinuteSpec
    , HourSpec
    , hourSpec
    , mkHourSpec
    , MonthSpec
    , monthSpec
    , mkMonthSpec
    , DayOfMonthSpec
    , dayOfMonthSpec
    , mkDayOfMonthSpec
    , DayOfWeekSpec
    , dayOfWeekSpec
    , mkDayOfWeekSpec
    , BaseField(..)
    , SpecificField
    , specificField
    , mkSpecificField
    , RangeField
    , rfBegin
    , rfEnd
    , mkRangeField
    , CronField(..)
    , StepField
    , sfField
    , sfStepping
    , mkStepField
    -- * Commonly Used Schedules
    , yearly
    , monthly
    , daily
    , weekly
    , hourly
    , everyMinute
    ) where


-------------------------------------------------------------------------------
import qualified Data.Foldable      as FT
import           Data.Ix
import           Data.List          (intercalate)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Text          (Text, unpack)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Shorthand schedules
-------------------------------------------------------------------------------


-- | Shorthand for every January 1st at midnight. Parsed with \@yearly, 0 0 1 1 *
yearly :: CronSchedule
yearly = monthly { month = Months (Field (SpecificField' (SpecificField 1))) }


-- | Shorthand for every 1st of the month at midnight. Parsed with \@monthly, 0 0 1 * *
monthly :: CronSchedule
monthly = daily { dayOfMonth = DaysOfMonth (Field (SpecificField' (SpecificField 1))) }


-- | Shorthand for every sunday at midnight. Parsed with \@weekly, 0 0 * * 0
weekly :: CronSchedule
weekly = daily { dayOfWeek = DaysOfWeek (Field (SpecificField' (SpecificField 0))) }


-- | Shorthand for every day at midnight. Parsed with \@daily, 0 0 * * *
daily :: CronSchedule
daily = hourly { hour = Hours (Field (SpecificField' (SpecificField 0))) }


-- | Shorthand for every hour on the hour. Parsed with \@hourly, 0 * * * *
hourly :: CronSchedule
hourly = everyMinute { minute = Minutes (Field (SpecificField' (SpecificField 0))) }


-- | Shorthand for an expression that always matches. Parsed with * * * * *
everyMinute :: CronSchedule
everyMinute = CronSchedule {
      minute     = Minutes (Field Star)
    , hour       = Hours (Field Star)
    , dayOfMonth = DaysOfMonth (Field Star)
    , month      = Months (Field Star)
    , dayOfWeek  = DaysOfWeek (Field Star)
    }


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------


-- | Specification for a cron expression
data CronSchedule = CronSchedule {
      minute     :: MinuteSpec     -- ^ Which minutes to run. First field in a cron specification.
    , hour       :: HourSpec       -- ^ Which hours to run. Second field in a cron specification.
    , dayOfMonth :: DayOfMonthSpec -- ^ Which days of the month to run. Third field in a cron specification.
    , month      :: MonthSpec      -- ^ Which months to run. Fourth field in a cron specification.
    , dayOfWeek  :: DayOfWeekSpec  -- ^ Which days of the week to run. Fifth field in a cron specification.
    } deriving (Eq)


instance Show CronSchedule where
  show cs = "CronSchedule " ++ showRaw cs


showRaw :: CronSchedule
           -> String
showRaw cs = unwords [ show (minute cs)
                     , show (hour cs)
                     , show (dayOfMonth cs)
                     , show (month cs)
                     , show (dayOfWeek cs)
                     ]


-------------------------------------------------------------------------------
-- | Crontab file, omitting comments.
newtype Crontab = Crontab {
      crontabEntries :: [CrontabEntry]
    } deriving (Eq)


instance Show Crontab where
  show (Crontab entries) = intercalate "\n" . map show $ entries



-------------------------------------------------------------------------------
newtype CronCommand = CronCommand {
      cronCommand :: Text
    } deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
-- | Essentially a line in a crontab file. It is either a schedule with a
-- command after it or setting an environment variable (e.g. FOO=BAR)
data CrontabEntry = CommandEntry CronSchedule CronCommand
                  | EnvVariable  Text Text
                  deriving (Eq)


instance Show CrontabEntry where
  show (CommandEntry s (CronCommand c)) = showRaw s ++ " " ++ unpack c
  show (EnvVariable n v)                = unpack n ++ "=" ++ unpack v


-------------------------------------------------------------------------------
-- | Minutes field of a cron expression
newtype MinuteSpec = Minutes {
      minuteSpec :: CronField
    } deriving (Eq)


instance Show MinuteSpec where
  show (Minutes cf) = show cf


--TODO: qc all of these
mkMinuteSpec :: CronField -> Maybe MinuteSpec
mkMinuteSpec cf
  | validCF cf 0 59 = Just (Minutes cf)
  | otherwise       = Nothing


-------------------------------------------------------------------------------
-- | Hours field of a cron expression
newtype HourSpec = Hours {
      hourSpec :: CronField
    } deriving (Eq)


instance Show HourSpec where
  show (Hours cf) = show cf


mkHourSpec :: CronField -> Maybe HourSpec
mkHourSpec cf
  | validCF cf 0 23 = Just (Hours cf)
  | otherwise       = Nothing


-------------------------------------------------------------------------------
-- | Day of month field of a cron expression
newtype DayOfMonthSpec = DaysOfMonth {
      dayOfMonthSpec :: CronField
    } deriving (Eq)


instance Show DayOfMonthSpec where
  show (DaysOfMonth cf) = show cf


mkDayOfMonthSpec :: CronField -> Maybe DayOfMonthSpec
mkDayOfMonthSpec cf
  | validCF cf 1 31 = Just (DaysOfMonth cf)
  | otherwise       = Nothing


-------------------------------------------------------------------------------
-- | Month field of a cron expression
newtype MonthSpec = Months {
      monthSpec :: CronField
    } deriving (Eq)


instance Show MonthSpec where
  show (Months cf) = show cf


mkMonthSpec :: CronField -> Maybe MonthSpec
mkMonthSpec cf
  | validCF cf 1 12 = Just (Months cf)
  | otherwise       = Nothing


-------------------------------------------------------------------------------
-- | Day of week field of a cron expression
newtype DayOfWeekSpec = DaysOfWeek {
      dayOfWeekSpec :: CronField
    } deriving (Eq)


instance Show DayOfWeekSpec where
  show (DaysOfWeek cf) = show cf


mkDayOfWeekSpec :: CronField -> Maybe DayOfWeekSpec
mkDayOfWeekSpec cf
  -- 0-7 is a matter of some debate but we'll be liberal here
  | validCF cf 0 7  = Just (DaysOfWeek cf)
  | otherwise       = Nothing


-------------------------------------------------------------------------------
validCF
    :: CronField
    -> Int
    -- ^ Min value
    -> Int
    -- ^ Max value
    -> Bool
validCF (Field bf) mn mx          = validBF bf mn mx
validCF (ListField bfs) mn mx     = FT.all (\bf -> validBF bf mn mx) bfs
validCF (StepField' (StepField bf step)) mn mx = validBF bf mn mx && inRange (mn, mx) step


-------------------------------------------------------------------------------
validBF
    :: BaseField
    -> Int
    -- ^ Min value
    -> Int
    -- ^ Max value
    -> Bool
validBF Star _ _ = True
validBF (SpecificField' (SpecificField n)) mn mx =
  inRange (mn, mx) n
validBF (RangeField' (RangeField n1 n2)) mn mx =
  inRange (mn, mx) n1 && inRange (mn, mx) n2


-------------------------------------------------------------------------------
-- | Individual field of a cron expression.
data BaseField = Star                         -- ^ Matches anything
               | SpecificField' SpecificField -- ^ Matches a specific value (e.g. 1)
               | RangeField' RangeField       -- ^ Matches a range of values (e.g. 1-3)
               deriving (Eq)


instance Show BaseField where
  show Star               = "*"
  show (SpecificField' f) = show f
  show (RangeField' rf)   = show rf


-------------------------------------------------------------------------------
newtype SpecificField = SpecificField {
      specificField :: Int
    } deriving (Eq)


instance Show SpecificField where
  show (SpecificField i) = show i


mkSpecificField :: Int -> Maybe SpecificField
mkSpecificField n
  | n >= 0 = Just (SpecificField n)
  | otherwise = Nothing


-------------------------------------------------------------------------------
data RangeField = RangeField {
      rfBegin :: Int
    , rfEnd   :: Int
    } deriving (Eq)


instance Show RangeField where
  show (RangeField x y) = show x ++ "-" ++ show y


mkRangeField :: Int -> Int -> Maybe RangeField
mkRangeField x y
  | x <= y    = Just (RangeField x y)
  | otherwise = Nothing



-------------------------------------------------------------------------------
data CronField = Field BaseField
               | ListField (NonEmpty BaseField) -- ^ Matches a list of expressions.
               | StepField' StepField           -- ^ Matches a stepped expression, e.g. (*/2).
                 deriving (Eq)


data StepField = StepField { sfField    :: BaseField
                           , sfStepping :: Int
                           } deriving (Eq)


instance Show StepField where
  show (StepField f step) = show f ++ "/" ++ show step

mkStepField :: BaseField -> Int -> Maybe StepField
mkStepField bf n
  | n > 0     = Just (StepField bf n)
  | otherwise = Nothing


instance Show CronField where
  show (Field f)       = show f
  show (ListField xs)  = intercalate "," . NE.toList . NE.map show $ xs
  show (StepField' sf) = show sf
