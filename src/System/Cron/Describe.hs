{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Cron.Describe
(
  -- Verbosity(..)
-- , displaySchedule
  describeTime
, describe
-- , description
-- , minuteDescriptor
) where

import Control.Monad
import System.Cron.Types
-- import Data.Char                                (toUpper)
import Data.List.NonEmpty                       (NonEmpty (..))
-- import qualified Data.List.NonEmpty as N hiding (NonEmpty (..))
import Data.List                                (intercalate)
import Data.Maybe                               (isJust, fromJust)
-- import Data.Text                                (Text)
-- import qualified Data.Text as T hiding          (Text)

leftPad :: Int -> String
leftPad n = if n < 10 then "0" ++ show n else show n

toHour :: Int -> String
toHour n = leftPad n ++ ":00"

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
    listPrefix       :: String
  , listSuffix       :: Maybe String
  , listRangeDisplay :: Int -> Int -> String
  , stepPrefix       :: Int -> String
  , stepSuffixSpec   :: BaseField -> Maybe String
  , displaySpec      :: Int -> String
  , displayStar      :: String
  , displayRange     :: Int -> Int -> String
  , describeItem     :: Int -> String
  }

-- minuteDescriptor :: Descriptor
-- minuteDescriptor = Descriptor {
--     allDescription      = "every minute"
--   , singleDescription   = minSingleDesc
--   , intervalDescription = minInterval
--   , intervalSuffix      = minSuffix
--   , betweenDescription  = minBetweenDesc
--   }
--   where minSuffix Star                = Nothing
--         minSuffix (SpecificField' sf) = Just . minSingleDesc $ specificField sf
--         minSuffix (RangeField' rf)    = Just $ minBetweenDesc (rfBegin rf) (rfEnd rf)
--         minSingleDesc n = "at " ++ show n ++ if n < 2 then " minute" else " minutes" ++ " past the hour"
--         minBetweenDesc b e = "between minutes " ++ show b ++ " and " ++ show e ++ " past the hour"
--         minInterval n = "every " ++ if n < 2 then " minute" else show n ++ " minutes" ++ " past the hour"

minuteDescriptor :: Descriptor
minuteDescriptor = Descriptor{
    listPrefix       = "at"
  , listSuffix       = Just "minutes past the hour"
  , listRangeDisplay = (\b e -> show b ++ " through " ++ show e)
  , stepPrefix       = (\n -> "every " ++ show n ++ " minutes")
  , stepSuffixSpec   = stepSuffix
  , displaySpec      = (\n -> "at " ++ show n ++ " minutes past the hour")
  , displayStar      = "every minute"
  , displayRange     = dispRange
  , describeItem     = show
  }
  where dispRange b e = "minutes " ++ show b ++ " through " ++ show e ++ " past the hour"
        stepSuffix (RangeField' rf)   = Just $ dispRange (rfBegin rf) (rfEnd rf)
        stepSuffix (SpecificField' s)
          | n == 0    = Nothing
          | otherwise = Just $ "starting at " ++ show n ++ " minutes past the hour"
          where n = specificField s
        stepSuffix Star               = Nothing

hourDescriptor :: Descriptor
hourDescriptor = Descriptor{
    listPrefix       = "at"
  , listSuffix       = Nothing
  , listRangeDisplay = (\b e -> dispItem b ++ " through " ++ dispItem e)
  , stepPrefix       = (\n -> "every " ++ show n ++ " hours")
  , stepSuffixSpec   = stepSuffix
  , displaySpec      = (\n -> "at " ++ dispItem n)
  , displayStar      = "every hour"
  , displayRange     = dispRange
  , describeItem     = dispItem
  }
  where dispRange b e = "between " ++ toHour b ++ " and " ++ toHour e
        dispItem n = leftPad n ++ ":00"
        stepSuffix (RangeField' rf)   = Just $ dispRange (rfBegin rf) (rfEnd rf)
        stepSuffix (SpecificField' s)
          | n == 0    = Nothing
          | otherwise = Just $ "starting at " ++ dispItem n
          where n = specificField s
        stepSuffix Star               = Nothing


    --  minSuffix Star                = Nothing
    --     minSuffix (SpecificField' sf) = Just . minSingleDesc $ specificField sf
    --     minSuffix (RangeField' rf)    = Just $ minBetweenDesc (rfBegin rf) (rfEnd rf)
    --     minSingleDesc n = "at " ++ show n ++ if n < 2 then " minute" else " minutes" ++ " past the hour"
    --     minBetweenDesc b e = "between minutes " ++ show b ++ " and " ++ show e ++ " past the hour"
    --     minInterval n = "every " ++ if n < 2 then " minute" else show n ++ " minutes" ++ " past the hour"
--
-- hourDescriptor :: Descriptor
-- hourDescriptor = Descriptor {
--     allDescription      = "every hour"
--   , singleDescription   = hourSingleDesc
--   , intervalDescription = hourInterval
--   , intervalSuffix      = hourSuffix
--   , betweenDescription  = hourBetweenDesc
--   }
--   where hourSuffix Star                = Nothing
--         hourSuffix (SpecificField' sf) = Just . hourSingleDesc $ specificField sf
--         hourSuffix (RangeField' rf)    = Just $ hourBetweenDesc (rfBegin rf) (rfEnd rf)
--         hourSingleDesc n = "at " ++ toHour n
--         hourBetweenDesc b e = "between " ++ toHour b ++ " and " ++ toHour e
--         hourInterval n = "every " ++ if n < 2 then " hour" else show n ++ " hours"
--         toHour n = leftPad n ++ ":00"

-- class Describe a where
--   descriptor :: a -> Descriptor
--
-- instance Describe MinuteSpec where
--   descriptor

describe :: CronField -> Descriptor -> String
describe (Field Star)               d = displayStar d
describe (Field (SpecificField' s)) d = displaySpec d $ specificField s
describe (Field (RangeField' rf))   d = displayRange d (rfBegin rf) (rfEnd rf)
describe (StepField' sf)            d =
  stepPrefix d (sfStepping sf) ++ maybe "" (", " ++) (stepSuffixSpec d $ sfField sf)
describe (ListField ls)             d =
  case describeListFields describeBF ls of
    Left  s -> s
    Right s -> listPrefix d ++ " " ++ maybe s ((s ++ ", ") ++) (listSuffix d)
  where
    describeBF Star               = displayStar d
    describeBF (RangeField' rf)   = listRangeDisplay d (rfBegin rf) (rfBegin rf)
    describeBF (SpecificField' s) = describeItem d $ specificField s

-- describe (StepField s)       d =
-- 	stepPrefix d s `append` stepSuffix s
-- describe (ListField ls)        d =
-- 	either L -> return L value
-- 		R -> list Prefix d `append` vs `append` listSuffix d
--
-- description :: CronField -> Descriptor -> String
-- description cf d = describeCronField cf
--   where
--     describeCronField (Field bf) = describeBaseField d bf
--
--     describeCronField (StepField' s) =
--       let interval = intervalDescription d $ sfStepping s
--           isuffix  = intervalSuffix d $ sfField s
--         in maybe interval ((interval ++ ", starting ") ++) isuffix
--
--     describeCronField (ListField ls) = describeListFields (describeBaseField d) ls
--
--     describeBaseField desc Star = allDescription desc
--
--     describeBaseField desc (RangeField' r) = betweenDescription desc (rfBegin r) (rfEnd r)
--
--     describeBaseField desc (SpecificField' s) = singleDescription desc $ specificField s

describeListFields :: (BaseField -> String) -> NonEmpty BaseField -> Either String String
describeListFields f (l :| ls) =
  fmap (intercalate ", ") . foldM describeF [] $ reverse (l:ls)
  where describeF _ Star = Left $ f Star
        describeF e bf   = Right $ f bf : e

describeTime :: MinuteSpec -> HourSpec -> String
describeTime (viewSF . minuteSpec -> Just m) (viewSF . hourSpec -> Just h) = "at " ++ formatTime m h
describeTime (viewRD . minuteSpec -> Just m) (viewSF . hourSpec -> Just h) = "every minute between " ++ time (rfBegin m) h ++ " and " ++ time (rfEnd m) h
describeTime (viewSF . minuteSpec -> Just m) (viewList . hourSpec -> Just h)  = describeMultHours m h
describeTime (minuteSpec -> m) (hourSpec -> h) =
  describe m minuteDescriptor ++ ", " ++ describe h hourDescriptor

describeMultHours :: SpecificField -> NonEmpty BaseField -> String
describeMultHours minuteSF ls@(bf :| bfs)
  | all isJust formattedTimes = "at " ++ intercalate ", " (map fromJust formattedTimes)
  | otherwise = describe (Field (SpecificField' minuteSF)) minuteDescriptor ++
                ", " ++
                describe (ListField ls) hourDescriptor
  where formattedTimes = map formatBaseField (bf : bfs)
        formatBaseField (SpecificField' s) = Just $ formatTime minuteSF s
        formatBaseField _                  = Nothing

time :: Int -> SpecificField -> String
time minute' hourF = leftPad (specificField hourF) ++ ":" ++ leftPad minute'

formatTime :: SpecificField -> SpecificField -> String
formatTime minuteF hourF = time (specificField minuteF) hourF















-- newtype Placeholder = Placeholder {
--     getPlaceholder :: String
--   }
--
-- pluralize :: Placeholder -> Placeholder
-- pluralize (Placeholder s) = Placeholder $ s ++ "s"
--
-- displayBaseField :: Placeholder -> BaseField -> String
-- displayBaseField ph Star =
--   unwords ["every", getPlaceholder ph]
-- displayBaseField ph (SpecificField' s) =
--   unwords ["at", getPlaceholder ph, show $ specificField s]
-- displayBaseField ph (RangeField' rf) =
--   unwords ["from", getPlaceholder ph, show $ rfBegin rf, "to", getPlaceholder ph, show $ rfEnd rf]
--
-- stepPrefix :: Placeholder -> Int -> [String]
-- stepPrefix ph n = ["every", show n, getPlaceholder $ pluralize ph]
--
-- displayStepField :: Placeholder -> StepField -> String
-- displayStepField ph sf = displayFields (sfField sf) (sfStepping sf)
--   where
--     displayFields Star n = unwords $ stepPrefix ph n
--     displayFields (SpecificField' s) n =
--       unwords $ stepPrefix ph n ++ ["starting at", getPlaceholder ph, show $ specificField s]
--     displayFields (RangeField' rf) n =
--       unwords $ stepPrefix ph n ++ ["from", getPlaceholder ph, show $ rfBegin rf, "to", getPlaceholder ph, show $ rfEnd rf]
--
-- displayCronField :: Placeholder -> CronField -> String
-- displayCronField ph (Field bf) = displayBaseField ph bf
-- displayCronField ph (ListField (a :| as)) =
--   intercalate ", " $ displayBaseField ph a : map (displayBaseField ph) as
-- displayCronField ph (StepField' sf) = displayStepField ph sf

-- data DisplayableField = forall a. (HasCronField a, Display a) => Displayable a
--
-- hide :: (HasCronField a, Display a) => a -> DisplayableField
-- hide = Displayable

-- class HasCronField a where
--   getCronField :: a -> CronField
--
-- instance HasCronField MinuteSpec where
--   getCronField = minuteSpec
--
-- instance HasCronField HourSpec where
--   getCronField = hourSpec
--
-- instance HasCronField DayOfMonthSpec where
--   getCronField = dayOfMonthSpec
--
-- instance HasCronField MonthSpec where
--   getCronField = monthSpec
--
-- instance HasCronField DayOfWeekSpec where
--   getCronField = dayOfWeekSpec

-- class Display a where
--   display :: a -> String
--
-- instance Display MinuteSpec where
--   display = displayCronField (Placeholder "minute") . minuteSpec
--
-- instance Display HourSpec where
--   display = displayCronField (Placeholder "hour") . hourSpec
--
-- instance Display DayOfMonthSpec where
--   display = displayCronField (Placeholder "day") . dayOfMonthSpec
--
-- instance Display MonthSpec where
--   display = displayCronField (Placeholder "month") . monthSpec
--
-- instance Display DayOfWeekSpec where
--   display = displayCronField (Placeholder "weekday") . dayOfWeekSpec
--
-- data Verbosity = Verbose | NonVerbose

-- -- | Given a verbosity level and a CronSchedule, convert it into a
-- -- human readable string
-- displaySchedule :: Verbosity -> CronSchedule -> String
-- displaySchedule v cs =
--   cap . intercalate ", " . getList . N.map unwrapDisplay . verbosify v $
--       hide (minute cs) :|
--     [
--       hide $ hour cs
--     , hide $ dayOfMonth cs
--     , hide $ month cs
--     , hide $ dayOfWeek cs
--     ]
--   where
--     getList (x :| xs) = x : xs
--
--     cap []         = []
--     cap (x : xs)   = toUpper x : xs
--
--     verbosify NonVerbose (c :| cfs) = c :| dropWhileEnd unwrapIsStar cfs
--     verbosify Verbose    cfs        = cfs
--
--     isStar (Field Star) = True
--     isStar _            = False
--
--     unwrapDisplay (Displayable a) = display a
--     unwrapIsStar  (Displayable a) = isStar $ getCronField a
