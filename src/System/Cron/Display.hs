{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}

module System.Cron.Display
(
  Verbosity(..)
, displaySchedule
, describeTime
) where

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

viewL :: CronField -> Maybe (NonEmpty BaseField)
viewL (ListField l) = Just l
viewL _             = Nothing

-- hasNoSpecialChars :: CronField -> Bool
-- hasNoSpecialChars (Field (SpecificField' _)) = True
-- hasNoSpecialChars _                          = False
--
-- isRangeDescriptor :: CronField -> Bool
-- isRangeDescriptor (Field (RangeField' _)) = True
-- isRangeDescriptor _                       = False
--
-- isListDescriptor :: CronField -> Bool
-- isListDescriptor (ListField _) = True
-- isListDescriptor _             = False

describeTime :: MinuteSpec -> HourSpec -> String
describeTime (viewSF . minuteSpec -> Just m) (viewSF . hourSpec -> Just h) = "at " ++ formatTime m h
describeTime (viewRD . minuteSpec -> Just m) (viewSF . hourSpec -> Just h) = "every minute between " ++ time (rfBegin m) h ++ " and " ++ time (rfEnd m) h
describeTime (viewSF . minuteSpec -> Just m) (viewL . hourSpec -> Just h)  = describeMultHours m h
describeTime _ _ = undefined
-- describeTime mins hours
--   | hasNoSpecialChars minCF && hasNoSpecialChars hourCF = undefined--mkTime minCF hourCF
--   | isRangeDescriptor minCF && hasNoSpecialChars hourCF = undefined
--   | hasNoSpecialChars minCF && isListDescriptor  hourCF = undefined
--   | otherwise                                           = undefined --describe minCF hourCF
  -- where
  --   minCF  = minuteSpec mins
  --   hourCF = hourSpec hours
    -- describeFields (Field (SpecificField' mf)) (Field (SpecificField' hf)) =
    --   formatTime mf hf

describeMultHours :: SpecificField -> NonEmpty BaseField -> String
describeMultHours minuteSF (h :| hs) = either id (("at " ++) . intercalate ", ") $ describe (h : hs)
  -- | hasStar (h : hs) = "at " ++ specificField minuteSf ++ " minutes past the hour, every hour"
  -- | otherwise
  where -- hasStar = elem (\Star -> True)
    describe []       = Right []
    describe (Star:_) = Left $ "at " ++ show (specificField minuteSF) ++ " minutes past the hour, every hour"
    describe (o:os)   = Right . (:) (formatBaseField o) =<< describe os
      where formatBaseField (SpecificField' s) = formatTime minuteSF s
            formatBaseField (RangeField' r)    = show (specificField minuteSF) ++ " minutes past the hour, between " ++ show (rfBegin r) ++ " and " ++ show (rfEnd r)
    -- describe ((SpecificField' s):hs) = describe =<< describe hs
    -- describe ((RangeField' r):hs)    = describe =<< describe hs


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
