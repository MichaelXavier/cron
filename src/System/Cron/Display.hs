{-# LANGUAGE ExistentialQuantification #-}

module System.Cron.Display
(
  Verbosity(..)
, displaySchedule
) where

import System.Cron.Types
import Data.Char           (toUpper)
import Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty as N hiding (NonEmpty (..))
import Data.List           (intercalate, dropWhileEnd)

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

data DisplayableField = forall a. (HasCronField a, Display a) => Displayable a

hide :: (HasCronField a, Display a) => a -> DisplayableField
hide = Displayable

class HasCronField a where
  getCronField :: a -> CronField

instance HasCronField MinuteSpec where
  getCronField = minuteSpec

instance HasCronField HourSpec where
  getCronField = hourSpec

instance HasCronField DayOfMonthSpec where
  getCronField = dayOfMonthSpec

instance HasCronField MonthSpec where
  getCronField = monthSpec

instance HasCronField DayOfWeekSpec where
  getCronField = dayOfWeekSpec

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
displaySchedule v cs =
  format . intercalate ", " . getList . N.map unwrapDisplay . verbosify v $
      hide (minute cs) :|
    [
      hide $ hour cs
    , hide $ dayOfMonth cs
    , hide $ month cs
    , hide $ dayOfWeek cs
    ]
  where
    getList (x :| xs) = x : xs

    format []         = []
    format (x : xs)   = toUpper x : xs

    verbosify NonVerbose (c :| cfs) = c :| dropWhileEnd unwrapIsStar cfs
    verbosify Verbose    cfs        = cfs

    isStar (Field Star) = True
    isStar _            = False

    unwrapDisplay (Displayable a) = display a
    unwrapIsStar  (Displayable a) = isStar $ getCronField a
