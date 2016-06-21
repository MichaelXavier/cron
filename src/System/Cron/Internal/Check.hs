{-# LANGUAGE RecordWildCards #-}
module System.Cron.Internal.Check where

-------------------------------------------------------------------------------
import           Control.Applicative         as A
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
-- Schedule projection
-------------------------------------------------------------------------------


-- | Will return the next time from the given starting point where
-- this schedule will match. Returns Nothing if the schedule will
-- never match. Note that this function is not inclusive of the given
-- time: the result will always be at least 1 minute beyond the given
-- time. This is usually used to implement absolute timestamp
-- schedulers. If you need to see multiple matches ahead, just keep
-- feeding the result into nextMatch. Note that because nextMatch only
-- returns Nothing on a schedule that will *never* be matched, it is
-- safe to assume that if a schedule returns a Just once, it will
-- always return a Just.
nextMatch :: CronSchedule -> UTCTime -> Maybe UTCTime
nextMatch cs@CronSchedule {..} now
  | domRestricted && dowRestricted = do
      -- this trick is courtesy of Python's croniter: run the schedule
      -- once with * in the DOM spot and once with * in the DOW slot
      -- and then choose the earlier of the two.
      domStarSpec <- mkDayOfMonthSpec (Field Star)
      dowStarSpec <- mkDayOfWeekSpec (Field Star)
      let domStarResult = nextMatch cs { dayOfMonth = domStarSpec } now
      let dowStarResult = nextMatch cs { dayOfWeek = dowStarSpec} now
      listToMaybe (sort (catMaybes [domStarResult, dowStarResult]))
  | otherwise = do
    expanded@Expanded {..} <- expand cs
    let daysSource = validDays monthF domF startDay
    listToMaybe (nextMatches daysSource expanded now)
  where
    UTCTime startDay _ = addUTCTime 60 now
    domRestricted = restricted (dayOfMonthSpec dayOfMonth)
    dowRestricted = restricted (dayOfWeekSpec dayOfWeek)


-------------------------------------------------------------------------------
nextMatches :: [Day] -> Expanded -> UTCTime -> [UTCTime]
nextMatches daysSource Expanded {..} now = solutions
  where
    -- move to next minute
    solutions = filter validSolution [UTCTime d tod
                                     | d <- daysSource
                                     , tod <- validTODs hourF minF
                                     ]
    validSolution t = t > now && dowMatch t dowF


-------------------------------------------------------------------------------
dowMatch :: UTCTime -> EField -> Bool
dowMatch (UTCTime d _) dows = (getDOW d `FT.elem` dows)


-------------------------------------------------------------------------------
-- | ISO8601 maps Sunday as 7 and Monday as 1, we want Sunday as 0
getDOW :: Day -> Int
getDOW d
  | iso8601DOW == 7 = 0
  | otherwise       = iso8601DOW
  where
    (_, _, iso8601DOW) = toWeekDate d


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
-- | Guarantees: the Expanded will be satisfiable (no invalid dates,
-- no empties). dow 7 will be normalized to 0 (Sunday)
expand :: CronSchedule -> Maybe Expanded
expand CronSchedule {..} = do
  expanded <- Expanded A.<$> minF'
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
    domRestricted = restricted (dayOfMonthSpec dayOfMonth)
    dowRestricted = restricted (dayOfWeekSpec dayOfWeek)
    -- If DOM and DOW are restricted, they are ORed, so even if
    -- there's an invalid day for the month, it is still satisfiable
    -- because it will just choose the DOW path
    satisfiable Expanded {..} = (domRestricted && dowRestricted) ||
      or [hasValidForMonth m domF | m <- (FT.toList monthF)]


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
expandBFStepped (_, unitMax) (SpecificField' sf) step =
  expandBFStepped (startAt, unitMax) Star step
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
  where
    nums = [ start + (step * iter) | iter <- [0..]]


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
hasValidForMonth 1 days  = FT.minimum days <= 31
hasValidForMonth 2 days  = FT.minimum days <= 29
hasValidForMonth 3 days  = FT.minimum days <= 31
hasValidForMonth 4 days  = FT.minimum days <= 30
hasValidForMonth 5 days  = FT.minimum days <= 31
hasValidForMonth 6 days  = FT.minimum days <= 30
hasValidForMonth 7 days  = FT.minimum days <= 31
hasValidForMonth 8 days  = FT.minimum days <= 31
hasValidForMonth 9 days  = FT.minimum days <= 30
hasValidForMonth 10 days = FT.minimum days <= 31
hasValidForMonth 11 days = FT.minimum days <= 30
hasValidForMonth 12 days = FT.minimum days <= 31
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



-- | Does the given cron schedule match for the given timestamp? This
-- is usually used for implementing polling-type schedulers like cron
-- itself.
scheduleMatches
    :: CronSchedule
    -> UTCTime
    -> Bool
scheduleMatches cs@CronSchedule {..} (UTCTime d t) =
  maybe False go (expand cs)
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
    (_, mth, dom) = toGregorian d
    (hr, mn) = timeOfDay t
    dow = getDOW d


restricted :: CronField -> Bool
restricted = not . isStar

isStar :: CronField -> Bool
isStar (Field Star)    = True
isStar (ListField bfs) = FT.any (== Star) bfs
isStar (StepField' sf) = sfField sf == Star && sfStepping sf == 1
isStar _               = False
