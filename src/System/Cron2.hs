{-# LANGUAGE RecordWildCards #-}
--TODO: internal module, quickcheck
module System.Cron2
    ( module System.Cron.Types
    , yearly
    , monthly
    , daily
    , weekly
    , hourly
    , everyMinute
    , scheduleMatches
    , nextMatch
    ) where


-------------------------------------------------------------------------------
import qualified Data.Foldable               as FT
import           Data.List
import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.List.NonEmpty          as NE
import           Data.Maybe
import           Data.Semigroup              (sconcat)
import           Data.Time
import           Data.Time.Calendar.WeekDate
import qualified Data.Traversable            as FT
-------------------------------------------------------------------------------
import           System.Cron.Types
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Shorthand schedules
-------------------------------------------------------------------------------


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
everyMinute = CronSchedule {
      minute     = fromJust . mkMinuteSpec . Field $ Star
    , hour       = fromJust . mkHourSpec . Field $ Star
    , dayOfMonth = fromJust . mkDayOfMonthSpec . Field $ Star
    , month      = fromJust . mkMonthSpec . Field $ Star
    , dayOfWeek  = fromJust . mkDayOfWeekSpec . Field $ Star
    }


-------------------------------------------------------------------------------
-- Schedule projection
-------------------------------------------------------------------------------

nextMatch :: CronSchedule -> UTCTime -> Maybe UTCTime
nextMatch cs = listToMaybe . nextMatches cs


-------------------------------------------------------------------------------
-- | Produces a lazy, *infinite* list of matching times in the future
-- for a schedule. If the schedule is not satisfiable, it will be an
-- empty list.
nextMatches :: CronSchedule -> UTCTime -> [UTCTime]
nextMatches cs t = maybe [] (flip nextMatches' t) (expand cs)


-------------------------------------------------------------------------------
nextMatches' :: Expanded -> UTCTime -> [UTCTime]
nextMatches' Expanded {..} now = solutions
  where
    -- move to next minute
    UTCTime startDay _ = addUTCTime 60 now
    solutions = filter validSolution [UTCTime d tod
                                     | d <- validDays monthF domF startDay
                                     , tod <- validTODs hourF minF
                                     ]
    validSolution t = t > now && dowMatch t dowF


-------------------------------------------------------------------------------
dowMatch :: UTCTime -> EField -> Bool
dowMatch (UTCTime d _) dows = oneIndexedDOW - 1 `elem` dows
  where
    (_, _, oneIndexedDOW) = toWeekDate d


-------------------------------------------------------------------------------
validDays :: EField -> EField -> Day -> [Day]
validDays months days start =
  concat (firstYearDates:subsequentYearDates)
  where
    (startYear, startMonth, _) = toGregorian start
    firstYearMonths = dropWhile (< startMonth) subsequentYearMonths
    subsequentYearMonths = sortBy compare (FT.toList months)
    firstYearDates = dateSequence firstYearMonths startYear
    subsequentYearDates = [ dateSequence subsequentYearMonths y | y <- [startYear+1..]]
    dateSequence mseq y = catMaybes [fromGregorianValid y m d
                                    | m <- mseq
                                    , d <- sortBy compare (FT.toList days)]


-------------------------------------------------------------------------------
-- | Guarantees: the Expanded will be satisfiable (no invalid dates, no empties). dow 7 will be normalized to 0 (Sunday)
expand :: CronSchedule -> Maybe Expanded
expand CronSchedule {..} = do
  expanded <- Expanded <$> minF'
                       <*> hourF'
                       <*> domF'
                       <*> monthF'
                       <*> dowF'
  if satisfiable expanded
     then Just expanded
     else Nothing
  where
    minF' = expandF (0, 59) (minuteSpec minute)
    hourF' = expandF (0, 23) (hourSpec hour)
    domF' = expandF (1, 31) (dayOfMonthSpec dayOfMonth)
    monthF' = expandF (1, 12) (monthSpec month)
    dowF' = remapSunday <$> expandF (0, 7) (dayOfWeekSpec dayOfWeek)
    remapSunday lst = case NE.partition (\n -> n == 0 || n == 7) lst of
                        ([], _)       -> lst
                        (_, noSunday) -> 0 :| noSunday
    satisfiable Expanded {..} = or [hasValidForMonth m domF | m <- (FT.toList monthF)]


-------------------------------------------------------------------------------
expandF :: (Int, Int) -> CronField -> Maybe EField
expandF rng (Field f)       = expandBF rng f
expandF rng (ListField fs)  = NE.nub . sconcat <$> FT.mapM (expandBF rng) fs
expandF rng (StepField' sf) = expandBFStepped rng (sfField sf) (sfStepping sf)


-------------------------------------------------------------------------------
expandBFStepped :: (Int, Int) -> BaseField -> Int -> Maybe EField
expandBFStepped rng Star step = NE.nonEmpty (fillTo rng step)
expandBFStepped (_, unitMax) (RangeField' rf) step = NE.nonEmpty (fillTo (start, finish') step)
  where
    finish' = min finish unitMax
    start = rfBegin rf
    finish = rfEnd rf
expandBFStepped rng (SpecificField' sf) step =
  NE.nonEmpty . NE.dropWhile (< startAt) =<< (expandBFStepped rng Star step)
  where
    startAt = specificField sf


-------------------------------------------------------------------------------
fillTo :: (Int, Int)
       -> Int
       -> [Int]
fillTo (start, finish) step
  | step <= 0      = []
  | finish < start = []
  | otherwise      = takeWhile (<= finish) nums
  where nums = map (start +) adds
        adds = map (*2) [0..]

-------------------------------------------------------------------------------
expandBF :: (Int, Int) -> BaseField -> Maybe EField
expandBF (lo, hi) Star         = Just (NE.fromList (enumFromTo lo hi))
expandBF _ (SpecificField' sf) = Just (specificField sf :| [])
expandBF _ (RangeField' rf)    = Just (NE.fromList (enumFromTo (rfBegin rf) (rfEnd rf)))


-------------------------------------------------------------------------------
validTODs :: EField -> EField -> [DiffTime]
validTODs hrs mns = dtSequence
  where
    minuteSequence = sortBy compare (FT.toList mns)
    hourSequence = sortBy compare (FT.toList hrs)
    -- order here ensures we'll count up minutes before hours
    dtSequence = [ todToDiffTime hr mn | hr <- hourSequence, mn <- minuteSequence]


-------------------------------------------------------------------------------
todToDiffTime :: Int -> Int -> DiffTime
todToDiffTime nextHour nextMin = fromIntegral ((nextHour * 60 * 60) + nextMin * 60)


-------------------------------------------------------------------------------
timeOfDay :: DiffTime -> (Int, Int)
timeOfDay t = (h, m)
  where
    seconds = floor t
    minutes = seconds `div` 60
    (h, m) = minutes `divMod` 60


-------------------------------------------------------------------------------
hasValidForMonth
    :: Int
    -- ^ Month
    -> EField
    -> Bool
hasValidForMonth 1 days  = minimum days <= 31
hasValidForMonth 2 days  = minimum days <= 29
hasValidForMonth 3 days  = minimum days <= 31
hasValidForMonth 4 days  = minimum days <= 30
hasValidForMonth 5 days  = minimum days <= 31
hasValidForMonth 6 days  = minimum days <= 30
hasValidForMonth 7 days  = minimum days <= 31
hasValidForMonth 8 days  = minimum days <= 31
hasValidForMonth 9 days  = minimum days <= 30
hasValidForMonth 10 days = minimum days <= 31
hasValidForMonth 11 days = minimum days <= 30
hasValidForMonth 12 days = minimum days <= 31
hasValidForMonth _ _     = False


-------------------------------------------------------------------------------
data Expanded = Expanded {
     minF   :: EField
   , hourF  :: EField
   , domF   :: EField
   , monthF :: EField
   , dowF   :: EField
   } deriving (Show)


-------------------------------------------------------------------------------
-- This could be an intmap but I'm not convinced there's significant
-- performance to be gained
type EField = NonEmpty Int


-------------------------------------------------------------------------------
-- Schedule checking
-------------------------------------------------------------------------------


scheduleMatches
    :: CronSchedule
    -> UTCTime
    -> Bool
scheduleMatches cs@CronSchedule {..} (UTCTime d t) = maybe False go (expand cs)
  where
    go Expanded {..} = and
      [ FT.elem mn minF
      , FT.elem hr hourF
      , FT.elem mth monthF
      , checkDOMAndDOW
      ]
      where
        -- turns out if neither dom and dow are stars, you're supposed to
        -- OR and not AND them:
        --
        -- Note: The day of a command's execution can
        -- be specified by two fields — day of month, and day of week. If
        -- both fields are restricted (i.e., aren't *), the command will
        -- be run when either field matches the current time. For example,
        -- ``30 4 1,15 * 5'' would cause a command to be run at 4:30 am on
        -- the 1st and 15th of each month, plus every Friday. One can,
        -- however, achieve the desired result by adding a test to the
        -- command (see the last example in EXAMPLE CRON FILE below).
        checkDOMAndDOW
          | restricted (dayOfMonthSpec dayOfMonth) && restricted (dayOfWeekSpec dayOfWeek) = 
              domMatches || dowMatches
          | otherwise = domMatches && dowMatches
        domMatches = FT.elem dom domF
        dowMatches = FT.elem dow dowF
        restricted = not . isStar
        isStar (Field Star) = True
        isStar _            = False
    (_, mth, dom) = toGregorian d
    (hr, mn) = timeOfDay t
    (_, _, iso8601DOW) = toWeekDate d
    dow
      | iso8601DOW == 7 = 0
      | otherwise       = iso8601DOW
