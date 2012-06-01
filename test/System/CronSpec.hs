module System.CronSpec (spec) where
-- TODO: this *should* just work with {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Test.Hspec.Monadic

import System.Cron

spec :: Spec
spec = sequence_ [ describeScheduleMatches ]

---- Specs
describeScheduleMatches :: Spec
describeScheduleMatches = describe "ScheduleMatches" $ do
  it "matches a catch-all" $
    scheduleMatches stars (day 5 25 1 2)

  it "matches a specific field" $
    scheduleMatches stars { hour = Hours (SpecificField 1)}
                    (day 5 25 1 2)

  it "matches a range" $
    scheduleMatches stars { dayOfMonth = DaysOfMonth (Range 3 5)}
                    (day 5 4 1 2)

  it "does not match invalid range" $
    not $ scheduleMatches stars { dayOfMonth = DaysOfMonth (Range 5 3)}
                          (day 5 4 1 2)

  it "matches a list" $
    scheduleMatches stars { month = Months (ListField [SpecificField 1,
                                                       SpecificField 2,
                                                       SpecificField 3])}
                    (day 2 3 1 2)
  it "matches a step field" $
     scheduleMatches stars { dayOfMonth = DaysOfMonth (DividedField (Range 10 16) 2)}
                     (day 5 12 1 2)
  it "does not match something missing the step field" $
    not $ scheduleMatches stars { dayOfMonth = DaysOfMonth (DividedField (Range 10 16) 2)}
                          (day 5 13 1 2)

  it "matches starred stepped fields" $
    scheduleMatches stars { minute = Minutes (DividedField Star 2)}
                          (day 5 13 1 4)

  it "does not match fields that miss starred stepped fields" $
    not $ scheduleMatches stars { minute = Minutes (DividedField Star 2)}
                          (day 5 13 1 5)

  it "matches multiple fields at once" $
    scheduleMatches stars { minute     = Minutes (DividedField Star 2),
                            dayOfMonth = DaysOfMonth (SpecificField 3),
                            hour       = Hours (Range 10 14) }
                    (day 5 3 13 2)

  where day m d h mn = UTCTime (fromGregorian 2012 m d) (diffTime h mn)
        diffTime h mn = timeOfDayToTime $ TimeOfDay h mn 0
        stars = CronSchedule (Minutes Star)
                             (Hours Star)
                             (DaysOfMonth Star)
                             (Months Star)
                             (DaysOfWeek Star)
