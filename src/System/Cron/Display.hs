module System.Cron.Display
(
  Verbosity(..)
, displaySchedule
) where

import System.Cron.Types
import Data.Char           (toUpper)
import Data.List.NonEmpty  (NonEmpty (..))
import Data.List           (intercalate)

newtype Placeholder = Placeholder {
    getPlaceholder :: String
  }

pluralize :: Placeholder -> Placeholder
pluralize (Placeholder s) = Placeholder $ s ++ "s"

displayBaseField :: Placeholder -> BaseField -> String
displayBaseField ph Star =
  unwords ["every", getPlaceholder ph]
displayBaseField ph (SpecificField' s) =
  unwords ["at", getPlaceholder ph, show $ specificField s]
displayBaseField ph (RangeField' rf) =
  unwords ["from", getPlaceholder ph, show $ rfBegin rf, "to", getPlaceholder ph, show $ rfEnd rf]

stepPrefix :: Placeholder -> Int -> [String]
stepPrefix ph n = ["every", show n, getPlaceholder $ pluralize ph]

displayStepField :: Placeholder -> StepField -> String
displayStepField ph sf = displayFields (sfField sf) (sfStepping sf)
  where
    displayFields Star n = unwords $ stepPrefix ph n
    displayFields (SpecificField' s) n =
      unwords $ stepPrefix ph n ++ ["starting at", getPlaceholder ph, show $ specificField s]
    displayFields (RangeField' rf) n =
      unwords $ stepPrefix ph n ++ ["from", getPlaceholder ph, show $ rfBegin rf, "to", getPlaceholder ph, show $ rfEnd rf]

displayCronField :: Placeholder -> CronField -> String
displayCronField ph (Field bf) = displayBaseField ph bf
displayCronField ph (ListField (a :| as)) =
  intercalate ", " $ displayBaseField ph a : map (displayBaseField ph) as
displayCronField ph (StepField' sf) = displayStepField ph sf

class Display a where
  display :: a -> String

instance Display MinuteSpec where
  display = displayCronField (Placeholder "minute") . minuteSpec

instance Display HourSpec where
  display = displayCronField (Placeholder "hour") . hourSpec

instance Display DayOfMonthSpec where
  display = displayCronField (Placeholder "day") . dayOfMonthSpec

instance Display MonthSpec where
  display = displayCronField (Placeholder "month") . monthSpec

instance Display DayOfWeekSpec where
  display = displayCronField (Placeholder "weekday") . dayOfWeekSpec


data Verbosity = Verbose | NonVerbose

-- | Given a verbosity level and a CronSchedule, convert it into a
-- human readable string
displaySchedule :: Verbosity -> CronSchedule -> String
displaySchedule _ cs = format . intercalate ", " $
  display (minute cs)     :
  display (hour cs)       :
  display (dayOfMonth cs) :
  display (month cs)      :
  [display (dayOfWeek cs)]
  where
    format (x:xs) = toUpper x : xs
