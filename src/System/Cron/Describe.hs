{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module System.Cron.Describe
(
  Verbosity(..)
, describe
) where

import Control.Monad
import System.Cron.Types
import Data.List.NonEmpty                 (NonEmpty (..), toList)
import System.Cron.Internal.DescribeTypes
import System.Cron.Internal.DescribeUtils

describeRange :: RangeField -> Descriptor -> String
describeRange rf d = allWords [rangePrefix d,
                               displayItem d (rfBegin rf),
                               rangeJoiner d,
                               displayItem d (rfEnd rf),
                               rangeSuffix d]


describeBaseField :: Descriptor -> BaseField -> DescribedValue
describeBaseField d (RangeField' rf)   = Concrete $ describeRange rf d
describeBaseField d Star               = Every $ "every " ++ singularDesc d
describeBaseField d (SpecificField' s) =
  Concrete $ allWords [specificPrefix d,
                       displayItem d (specificField s),
                       specificSuffix d]


type StarOrDesc = Either String String

describeListFields :: (BaseField -> String) -> NonEmpty BaseField -> StarOrDesc
describeListFields f (l :| ls) =
  fmap joinWords . foldM describeF [] $ reverse (l:ls)
  where describeF _ Star = Left $ f Star
        describeF e bf   = Right $ f bf : e



describeCronField :: Descriptor -> CronField -> DescribedValue
describeCronField d (Field f) = describeBaseField d f


describeCronField d (StepField' sf) = Concrete $
  stepPrefix ++ maybe "" (", " ++) (stepSuffix $ sfField sf)
  where
    stepPrefix = unwords ["every", show (sfStepping sf), pluralDesc d]
    stepSuffix Star              = Nothing
    stepSuffix (RangeField' rf)  = Just $ describeRange rf d
    stepSuffix (SpecificField' s) = stepSpecificSuffix d $ specificField s


describeCronField d (ListField ls) =
  case describeListFields describeBF ls of
    Left  s -> Every s
    Right s -> Concrete $ unwords [listPrefix d,
                                   maybe s ((s ++ " ") ++) (listSuffix d)]
  where
    describeBF Star               = "every " ++ singularDesc d
    describeBF (SpecificField' s) = displayItem d $ specificField s
    describeBF (RangeField' rf)   = unwords [displayItem d (rfBegin rf),
                                             "through",
                                             displayItem d (rfEnd rf)]


describeTime :: MinuteSpec -> HourSpec -> Time
describeTime (viewSF . minuteSpec -> Just m) (viewSF . hourSpec -> Just h) =
  ConcreteTime $ "at " ++ formatTime m h
describeTime (viewRD . minuteSpec -> Just m) (viewSF . hourSpec -> Just h) =
  ConcreteTime $ unwords ["every minute between",
                          time (rfBegin m) h,
                          "and",
                          time (rfEnd m) h]
describeTime (viewSF . minuteSpec -> Just m) (viewList . hourSpec -> Just h) =
  describeMultHours m h
describeTime (minuteSpec -> m) (hourSpec -> h) =
  Other (return $ describeCronField minuteDescriptor m)
        (return $ describeCronField hourDescriptor h)


describeMultHours :: SpecificField -> NonEmpty BaseField -> Time
describeMultHours minuteSF ls =
  maybe otherTime (formatAllFields . toList) $ traverse formatBaseField ls
  where hourCF   = ListField ls
        minuteCF = Field (SpecificField' minuteSF)
        formatAllFields = ConcreteTime . ("at " ++) . joinWords
        otherTime = Other (return describedMinute)
                          (return $ describeCronField hourDescriptor hourCF)
        formatBaseField (SpecificField' s) = Just $ formatTime minuteSF s
        formatBaseField Star               = Nothing
        formatBaseField f@(RangeField' _)  =
          Just $ unwords [show describedMinute,
                          show $ describeCronField hourDescriptor (Field f)]
        describedMinute = describeCronField minuteDescriptor minuteCF

description :: CronSchedule -> Description
description cs = Desc (describeTime (minute cs) (hour cs))
                      (return ddom)
                      (return dm)
                      (return ddow)
  where ddom = describeCronField domDescriptor $ dayOfMonthSpec (dayOfMonth cs)
        dm   = describeCronField monthDescriptor $ monthSpec (month cs)
        ddow = describeCronField dowDescriptor $ dayOfWeekSpec (dayOfWeek cs)


matchVerbosity :: Verbosity -> Description -> Description
matchVerbosity v d@Desc{..} = d{  _dom   = stripEvery v =<< _dom
                                , _dow   = stripEvery v =<< _dow
                                , _time  = stripTime _time
                                , _month = stripEvery NotVerbose =<< _month}
  where stripTime t@(ConcreteTime _)   = t
        stripTime (Other mbMin mbHour) = Other mbMin (stripEvery v =<< mbHour)


stripEvery :: Verbosity -> DescribedValue -> Maybe DescribedValue
stripEvery NotVerbose (Every _) = Nothing
stripEvery _          c         = Just c


describe :: Verbosity -> CronSchedule -> String
describe v = cap . show . matchVerbosity v . description
