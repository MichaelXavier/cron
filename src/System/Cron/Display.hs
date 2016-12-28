{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}

module System.Cron.Display
(
  Verbosity(..)
, displaySchedule
, describeTime
, description
-- , minuteDescriptor
) where

import Control.Monad
import System.Cron.Types
import Data.Char           (toUpper)
import Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty as N hiding (NonEmpty (..))
import Data.List           (intercalate, dropWhileEnd)

leftPad :: Int -> String
leftPad n = if n < 10 then "0" ++ show n else show n

viewSF :: CronField -> Maybe SpecificField
viewSF (Field (SpecificField' s)) = Just s
viewSF _                          = Nothing

viewRD :: CronField -> Maybe RangeField
viewRD (Field (RangeField' r)) = Just r
viewRD _                       = Nothing

viewList :: CronField -> Maybe (NonEmpty BaseField)
viewList (ListField l) = Just l
viewList _             = Nothing

-- isStar :: CronField -> Bool
-- isStar (Field Star) = True
-- isStar _            = False

--
-- hasNoSpecialChars :: CronField -> Bool
-- hasNoSpecialChars (Field (SpecificField' _)) = True
-- hasNoSpecialChars _                          = False

-- viewStep :: CronField -> Maybe (StepField)
-- viewStep (StepField' s) = Just s
-- viewStep _              = Nothing

-- isRange :: CronField -> Bool
-- isRange (Field (RangeField' _)) = True
-- isRange _                       = False
--
-- isList :: CronField -> Bool
-- isList (ListField _) = True
-- isList _             = False
--
-- class Describe a where
--   allDescription      :: a -> String
--   singleDescription   :: a -> Int -> String
--   intervalDescription :: a -> String
--   betweenDescription  :: a -> String
--   descriptionFormat   :: a -> String
  -- cronField           :: a -> CronField

data Descriptor = Descriptor {
    allDescription      :: String
  , singleDescription   :: Int -> String
  , intervalDescription :: Int -> String
  , intervalSuffix      :: BaseField -> Maybe String
  , betweenDescription  :: Int -> Int -> String
  }

minuteDescriptor :: Descriptor
minuteDescriptor = Descriptor {
    allDescription      = "every minute"
  , singleDescription   = minSingleDesc
  , intervalDescription = minInterval
  , intervalSuffix      = minSuffix
  , betweenDescription  = minBetweenDesc
  }
  where minSuffix Star                = Nothing
        minSuffix (SpecificField' sf) = Just . minSingleDesc $ specificField sf
        minSuffix (RangeField' rf)    = Just $ minBetweenDesc (rfBegin rf) (rfEnd rf)
        minSingleDesc n = "at " ++ show n ++ if n < 2 then " minute" else " minutes" ++ " past the hour"
        minBetweenDesc b e = "between minutes " ++ show b ++ " and " ++ show e ++ " past the hour"
        minInterval n = "every " ++ if n < 2 then " minute" else show n ++ " minutes" ++ " past the hour"

hourDescriptor :: Descriptor
hourDescriptor = Descriptor {
    allDescription      = "every hour"
  , singleDescription   = hourSingleDesc
  , intervalDescription = hourInterval
  , intervalSuffix      = hourSuffix
  , betweenDescription  = hourBetweenDesc
  }
  where hourSuffix Star                = Nothing
        hourSuffix (SpecificField' sf) = Just . hourSingleDesc $ specificField sf
        hourSuffix (RangeField' rf)    = Just $ hourBetweenDesc (rfBegin rf) (rfEnd rf)
        hourSingleDesc n = "at " ++ toHour n
        hourBetweenDesc b e = "between " ++ toHour b ++ " and " ++ toHour e
        hourInterval n = "every " ++ if n < 2 then " hour" else show n ++ " hours"
        toHour n = leftPad n ++ ":00"

-- class Describe a where
--   descriptor :: a -> Descriptor
--
-- instance Describe MinuteSpec where
--   descriptor

description :: CronField -> Descriptor -> String
description cf d = describeCronField cf
  where
    describeCronField (Field bf) = describeBaseField d bf

    describeCronField (StepField' s) =
      let interval = intervalDescription d $ sfStepping s
          isuffix  = intervalSuffix d $ sfField s
        in maybe interval ((interval ++ ", starting ") ++) isuffix

    describeCronField (ListField ls) = describeListFields (describeBaseField d) ls

    describeBaseField desc Star = allDescription desc

    describeBaseField desc (RangeField' r) = betweenDescription desc (rfBegin r) (rfEnd r)

    describeBaseField desc (SpecificField' s) = singleDescription desc $ specificField s

describeListFields :: (BaseField -> String) -> NonEmpty BaseField -> String
describeListFields f (l :| ls) =
  ("at " ++) . either id (intercalate ", ") . foldM describe [] $ reverse (l:ls)
  where describe _ Star = Left $ f Star
        describe e bf   = Right $ f bf : e

describeTime :: MinuteSpec -> HourSpec -> String
describeTime (viewSF . minuteSpec -> Just m) (viewSF . hourSpec -> Just h) = "at " ++ formatTime m h
describeTime (viewRD . minuteSpec -> Just m) (viewSF . hourSpec -> Just h) = "every minute between " ++ time (rfBegin m) h ++ " and " ++ time (rfEnd m) h
describeTime (viewSF . minuteSpec -> Just m) (viewList . hourSpec -> Just h)  = describeMultHours m h
describeTime (minuteSpec -> m) (hourSpec -> h) =
  description m minuteDescriptor ++ ", " ++ description h hourDescriptor

describeMultHours :: SpecificField -> NonEmpty BaseField -> String
describeMultHours minuteSF = describeListFields (formatBaseField minuteSF)
  where formatBaseField msf (SpecificField' s) = formatTime msf s
        formatBaseField msf (RangeField' r)    = show (specificField msf) ++ " minutes past the hour, between " ++ leftPad (rfBegin r) ++ " and " ++ leftPad (rfEnd r)
        formatBaseField msf Star               = show (specificField msf) ++ " minutes past the hour, every hour"

time :: Int -> SpecificField -> String
time minute' hourF = leftPad (specificField hourF) ++ ":" ++ leftPad minute'

formatTime :: SpecificField -> SpecificField -> String
formatTime minuteF hourF = time (specificField minuteF) hourF















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
  cap . intercalate ", " . getList . N.map unwrapDisplay . verbosify v $
      hide (minute cs) :|
    [
      hide $ hour cs
    , hide $ dayOfMonth cs
    , hide $ month cs
    , hide $ dayOfWeek cs
    ]
  where
    getList (x :| xs) = x : xs

    cap []         = []
    cap (x : xs)   = toUpper x : xs

    verbosify NonVerbose (c :| cfs) = c :| dropWhileEnd unwrapIsStar cfs
    verbosify Verbose    cfs        = cfs

    isStar (Field Star) = True
    isStar _            = False

    unwrapDisplay (Displayable a) = display a
    unwrapIsStar  (Displayable a) = isStar $ getCronField a
