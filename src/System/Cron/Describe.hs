{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP             #-}
--------------------------------------------------------------------
-- |
-- Module      : System.Cron.Describe
-- Description : Turn a cron schedule into a human-readable string
-- Copyright   : (c) Joseph Canero 2016
-- License     : MIT
--
-- Maintainer: Joseph Canero <jmc41493@gmail.com>
-- Portability: portable
--
--
-- > import System.Cron
-- >
-- > main :: IO ()
-- > main = do
-- >   let Right cs1 = parseCronSchedule "*/2 * 3 * 4,5,6"
-- >   print $ describe defaultOpts cs1
-- >
-- >   let Right cs2 = parseCronSchedule "*/2 12 3 * 4,5,6"
-- >   print $ describe (twentyFourHourFormat <> verbose) cs2
--------------------------------------------------------------------
module System.Cron.Describe
    (
      -- * Options handling
      defaultOpts
    , twentyFourHourFormat
    , twelveHourFormat
    , verbose
    , notVerbose
    , OptionBuilder
      -- * Describe a CronSchedule
    , describe
    ) where

-------------------------------------------------------------------------------
import Control.Monad
import Data.List.NonEmpty                       (NonEmpty (..), toList)
import Data.Maybe                               (fromJust)
#if !MIN_VERSION_base(4,8,0)
import Data.Traversable                         (traverse)
#endif
-------------------------------------------------------------------------------
import System.Cron.Internal.Describe.Descriptors
import System.Cron.Internal.Describe.Options
import System.Cron.Internal.Describe.Time
import System.Cron.Internal.Describe.Types
import System.Cron.Internal.Describe.Utils
import System.Cron.Types
-------------------------------------------------------------------------------


-- | Given an 'OptionBuilder' and a 'CronSchedule' parsed with
-- 'System.Cron.Parser.parseCronSchedule', return a human-readable string
-- describing when that schedule will match.
describe :: OptionBuilder -> CronSchedule -> String
describe ob = cap                      .
              show                     .
              matchVerbosity verbosity .
              description timeFormat
  where Opts{..} = getOpts ob


-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------


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

-- There are a few special cases to handle when describing the minute and hour
-- fields that will make the cron description easier to read.
-- For the most part, these are pretty straight forward. The first three
-- pattern matches look for specific patterns in the minute and hour fields that
-- can be formatted differently. The last pattern match just defaults
-- to describing the fields using existing rules.
describeTime :: TimeFormat -> MinuteSpec -> HourSpec -> Time
describeTime tf (viewMinute -> Just m) (viewHour -> Just h) =
  ConcreteTime $ "at " ++ format tf m h
describeTime tf (viewMinuteRange -> Just (m1, m2)) (viewHour -> Just h) =
  ConcreteTime $ unwords ["every minute between",
                          format tf m1 h,
                          "and",
                          format tf m2 h]
describeTime tf (viewMinute -> Just m) (viewHourList -> Just hs) =
  describeMultHours tf m hs
describeTime tf (minuteSpec -> m) (hourSpec -> h) =
  Other (return $ describeCronField minuteDescriptor m)
        (return $ describeCronField (hourDescriptor tf) h)

-- We want to create a description for multiple hours given a concrete minute.
-- This is rather ugly, as the ListField type allows for any BaseField, so
-- we can potentially have a '*' within the list. In that case, we don't need
-- to describe the rest of the BaseFields for hour list, since we will just be
-- firing each hour.
describeMultHours :: TimeFormat -> Minute -> NonEmpty BaseField -> Time
describeMultHours t mn@(Minute m) ls =
  maybe mkOther (formatAllFields . toList) $ traverse formatBaseField ls
  where hourCF   = ListField ls
        minuteCF = Field (SpecificField' (fromJust $ mkSpecificField m))

        formatAllFields = ConcreteTime . ("at " ++) . joinWords

        formatBaseField (SpecificField' s) =
          Just $ format t mn (Hour (specificField s))
        formatBaseField Star               = Nothing
        formatBaseField f@(RangeField' _)  =
          Just $ unwords [show describedMinute,
                          show $ describeCronField (hourDescriptor t) (Field f)]

        mkOther = Other (return describedMinute)
                        (return $ describeCronField (hourDescriptor t) hourCF)
        describedMinute = describeCronField minuteDescriptor minuteCF


description :: TimeFormat -> CronSchedule -> Description
description t c = Desc (describeTime t (minute c) (hour c))
                       (return ddom)
                       (return dm)
                       (return ddow)
  where ddom = describeCronField domDescriptor $ dayOfMonthSpec (dayOfMonth c)
        dm   = describeCronField monthDescriptor $ monthSpec (month c)
        ddow = describeCronField dowDescriptor $ dayOfWeekSpec (dayOfWeek c)


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
