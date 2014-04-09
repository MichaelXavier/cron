{-# LANGUAGE OverloadedStrings #-}
module System.CronSpec (spec) where

import Control.Applicative
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import System.Cron

import Debug.Trace

spec :: Spec
spec = sequence_ [describeScheduleMatches,
                  describeCronScheduleShow,
                  describeCrontabEntryShow,
                  describeCrontabShow ]

---- Specs
describeScheduleMatches :: Spec
describeScheduleMatches = describe "ScheduleMatches" $ do
  it "matches a catch-all" $
    scheduleMatches stars (day 5 25 1 2)

  it "matches a specific field" $
    scheduleMatches stars { hour = Hours (SpecificField 1)}
                    (day 5 25 1 2)

  it "matches a range" $
    scheduleMatches stars { dayOfMonth = DaysOfMonth (RangeField 3 5)}
                    (day 5 4 1 2)

  it "does not match invalid range" $
    not $ scheduleMatches stars { dayOfMonth = DaysOfMonth (RangeField 5 3)}
                          (day 5 4 1 2)

  it "matches a list" $
    scheduleMatches stars { month = Months (ListField [SpecificField 1,
                                                       SpecificField 2,
                                                       SpecificField 3])}
                    (day 2 3 1 2)

  it "matches a step field" $
     scheduleMatches stars { dayOfMonth = DaysOfMonth (StepField (RangeField 10 16) 2)}
                     (day 5 12 1 2)

  it "does not match something missing the step field" $
    not $ scheduleMatches stars { dayOfMonth = DaysOfMonth (StepField (RangeField 10 16) 2)}
                          (day 5 13 1 2)

  it "matches starred stepped fields" $
    scheduleMatches stars { minute = Minutes (StepField Star 2)}
                          (day 5 13 1 4)

  it "does not match fields that miss starred stepped fields" $
    not $ scheduleMatches stars { minute = Minutes (StepField Star 2)}
                          (day 5 13 1 5)

  it "matches multiple fields at once" $
    scheduleMatches stars { minute     = Minutes (StepField Star 2),
                            dayOfMonth = DaysOfMonth (SpecificField 3),
                            hour       = Hours (RangeField 10 14) }
                    (day 5 3 13 2)

  it "matches a monday as 1" $
    scheduleMatches stars { dayOfWeek  = DaysOfWeek (SpecificField 1) }
                    (UTCTime (fromGregorian 2014 3 17) 0)

  it "matches a sunday as 0" $
    scheduleMatches stars { dayOfWeek  = DaysOfWeek (SpecificField 0) }
                    (UTCTime (fromGregorian 2014 3 16) 0)

  it "matches a sunday as 7" $
    scheduleMatches stars { dayOfWeek  = DaysOfWeek (SpecificField 7) }
                    (UTCTime (fromGregorian 2014 3 16) 0)

  it "matches weekly on a sunday at 0:00" $
    scheduleMatches weekly (UTCTime (fromGregorian 2014 4 6) 0)

  it "does not match weekly on a sunday at some time past midnight" $
    not $ scheduleMatches weekly (UTCTime (fromGregorian 2014 6 4) 600)

  it "does not match weekly on another day at midnight" $
    not $ scheduleMatches weekly (UTCTime (fromGregorian 2014 6 5) 600)

  prop "star matches everything" $ \t ->
    scheduleMatches stars t

  prop "exact time matches" $ arbitraryTimeFields $ \y m d h mn ->
    let sched = CronSchedule (Minutes $ SpecificField mn)
                             (Hours $ SpecificField h)
                             (DaysOfMonth $ SpecificField d)
                             (Months $ SpecificField m)
                             (DaysOfWeek Star)
        t     = day' y m d h mn
    in scheduleMatches sched t

  prop "any time with the same minute as n * * * * matches" $ arbitraryTimeFields $ \y m d h mn ->
    let sched = stars { minute = Minutes $ SpecificField mn }
        t     = day' y m d h mn
    in scheduleMatches sched t

  prop "any time with the diff minute as n * * * * does not match" $ arbitraryTimeFields $ \y m d h mn ->
    let sched = stars { minute = Minutes $ SpecificField $ stepMax 59 mn }
        t     = day' y m d h mn
    in not $ scheduleMatches sched t

  prop "any time with the same hour as * n * * * matches" $ arbitraryTimeFields $ \y m d h mn ->
    let sched = stars { hour = Hours $ SpecificField h }
        t     = day' y m d h mn
    in scheduleMatches sched t

  prop "any time with the diff hour as * n * * * does not match" $ arbitraryTimeFields $ \y m d h mn ->
    let sched = stars { hour = Hours $ SpecificField $ stepMax 23 h }
        t     = day' y m d h mn
    in not $ scheduleMatches sched t

  prop "any time with the same day as * * n * * matches" $ \t@(UTCTime dy dt) ->
    let (_, m, d) = toGregorian dy
        (h, mn)   = hoursMins dt
        sched = CronSchedule (Minutes $ SpecificField mn)
                             (Hours $ SpecificField h)
                             (DaysOfMonth $ SpecificField d)
                             (Months $ SpecificField m)
                             (DaysOfWeek Star)
    in scheduleMatches sched t

  prop "any time with the diff day as * * n * * does not match" $ arbitraryTimeFields $ \y m d h mn ->
    let sched = stars { dayOfMonth = DaysOfMonth $ SpecificField $ stepMax 31 d }
        t     = day' y m d h mn
    in not $ scheduleMatches sched t

  where day = day' 2012
        day' y m d h mn = UTCTime (fromGregorian y m d) (diffTime h mn)
        diffTime h mn = timeOfDayToTime $ TimeOfDay h mn 1

arbitraryTimeFields f y m d h mn = f (getPositive y)
                                     (min 12 $ getPositive m)
                                     (min 31 $ getPositive d)
                                     (min 23 $ getPositive h)
                                     (min 59 $ getPositive mn)

hoursMins uTime = (hr, mn)
  where
    TimeOfDay { todHour = hr,
                todMin  = mn} = timeToTimeOfDay uTime


stepMax :: (Enum a, Ord a) => a -> a -> a
stepMax mx n | n < mx    = succ n
             | otherwise = pred n


describeCronScheduleShow :: Spec
describeCronScheduleShow = describe "CronSchedule show" $ do
  it "formats stars" $
    show stars `shouldBe`
         "CronSchedule * * * * *"

  it "formats specific numbers" $
    show stars { dayOfWeek = DaysOfWeek (SpecificField 3)} `shouldBe`
         "CronSchedule * * * * 3"

  it "formats lists" $
    show stars { minute = Minutes (ListField [SpecificField 1,
                                   SpecificField 2,
                                   SpecificField 3])} `shouldBe`
         "CronSchedule 1,2,3 * * * *"

  it "formats ranges" $
    show stars { hour = Hours (RangeField 7 10)} `shouldBe`
         "CronSchedule * 7-10 * * *"

  it "formats steps" $
    show stars { dayOfMonth = DaysOfMonth (StepField (ListField [SpecificField 3, SpecificField 5]) 2)} `shouldBe`
         "CronSchedule * * 3,5/2 * *"

  it "formats @yearly" $
    show yearly `shouldBe` "CronSchedule 0 0 1 1 *"

  it "formats @monthly" $
    show monthly `shouldBe` "CronSchedule 0 0 1 * *"

  it "formats @weekly" $
    show weekly `shouldBe` "CronSchedule 0 0 * * 0"

  it "formats @daily" $
    show daily `shouldBe` "CronSchedule 0 0 * * *"

  it "formats @hourly" $
    show hourly `shouldBe` "CronSchedule 0 * * * *"

  it "formats everyMinute" $
    show everyMinute `shouldBe` "CronSchedule * * * * *"

describeCrontabShow :: Spec
describeCrontabShow = describe "Crontab Show" $ do
  it "prints nothing for an empty crontab" $
    show (Crontab []) `shouldBe` ""

describeCrontabEntryShow :: Spec
describeCrontabEntryShow = describe "CrontabEntry Show" $ do
  it "formats environment variable sets" $
    show envSet `shouldBe` "FOO=BAR"

  it "formats command entries" $
    show entry `shouldBe` "* * * * * do stuff"


envSet :: CrontabEntry
envSet = EnvVariable "FOO" "BAR"

entry :: CrontabEntry
entry = CommandEntry stars "do stuff"

stars :: CronSchedule
stars = CronSchedule (Minutes Star)
                     (Hours Star)
                     (DaysOfMonth Star)
                     (Months Star)
                     (DaysOfWeek Star)

instance Arbitrary UTCTime where
  arbitrary = do
    d <- ModifiedJulianDay . fromInteger . getPositive <$> arbitrary
    t <- fromInteger . getPositive <$> arbitrary
    return $ UTCTime d t

