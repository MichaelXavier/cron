{-# LANGUAGE OverloadedStrings #-}
module System.Test.Cron (tests) where

-------------------------------------------------------------------------------
import           Data.List.NonEmpty (NonEmpty (..))
-------------------------------------------------------------------------------
import           SpecHelper
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "System.Cron"
  [ describeScheduleMatches
  , describeCronScheduleShow
  , describeCrontabEntryShow
  , describeCrontabShow
  , describeNextMatch
  ]


describeScheduleMatches :: TestTree
describeScheduleMatches = testGroup "ScheduleMatches"
  [
      testCase "matches a catch-all" $
      scheduleMatches stars (day 5 25 1 2) @?= True

    , testCase "matches a specific field" $
      scheduleMatches stars { hour = mkHourSpec' (Field (SpecificField' (mkSpecificField' 1)))}
                      (day 5 25 1 2) @?= True

    , testCase "matches a range" $
      scheduleMatches stars { dayOfMonth = mkDayOfMonthSpec' (Field (RangeField' (mkRangeField' 3 5)))}
                      (day 5 4 1 2) @?= True

    , testCase "matches a list" $
      scheduleMatches stars { month = mkMonthSpec' (ListField (SpecificField' (mkSpecificField' 1) :| [SpecificField' (mkSpecificField' 2), SpecificField' (mkSpecificField' 3)]))}
                     (day 2 3 1 2) @?= True

    , testCase "matches a step field" $
       scheduleMatches stars { dayOfMonth = mkDayOfMonthSpec' (StepField' (mkStepField' (RangeField' (mkRangeField' 10 16)) 2))}
                       (day 5 12 1 2) @?= True

    , testCase "does not match something missing the step field" $
      scheduleMatches stars { dayOfMonth = mkDayOfMonthSpec' (StepField' (mkStepField' (RangeField' (mkRangeField' 10 16)) 2))}
                      (day 5 13 1 2) @?= False

    , testCase "matches starred stepped fields" $
      scheduleMatches stars { minute = mkMinuteSpec' (StepField' (mkStepField' Star 2))}
                            (day 5 13 1 4) @?= True

    , testCase "does not match fields that miss starred stepped fields" $
      scheduleMatches stars { minute = mkMinuteSpec' (StepField' (mkStepField' Star 2))}
                      (day 5 13 1 5) @?= False

    , testCase "matches multiple fields at once" $
      scheduleMatches stars { minute     = mkMinuteSpec' (StepField' (mkStepField' Star 2)),
                              dayOfMonth = mkDayOfMonthSpec' (Field (SpecificField' (mkSpecificField' 3))),
                              hour       = mkHourSpec' (Field (RangeField' (mkRangeField' 10 14))) }
                      (day 5 3 13 2) @?= True

    , testCase "matches a monday as 1" $
      scheduleMatches stars { dayOfWeek  = mkDayOfWeekSpec' (Field (SpecificField' (mkSpecificField' 1))) }
                      (UTCTime (fromGregorian 2014 3 17) 0) @?= True

    , testCase "matches a sunday as 0" $
      scheduleMatches stars { dayOfWeek  = mkDayOfWeekSpec' (Field (SpecificField' (mkSpecificField' 0))) }
                      (UTCTime (fromGregorian 2014 3 16) 0) @?= True

    , testCase "matches a sunday as 7" $
      scheduleMatches stars { dayOfWeek  = mkDayOfWeekSpec' (Field (SpecificField' (mkSpecificField' 7))) }
                      (UTCTime (fromGregorian 2014 3 16) 0) @?= True

    , testCase "matches weekly on a sunday at 0:00" $
      scheduleMatches weekly (UTCTime (fromGregorian 2014 4 6) 0) @?= True

    , testCase "does not match weekly on a sunday at some time past midnight" $
      scheduleMatches weekly (UTCTime (fromGregorian 2014 6 4) 600) @?= False

    , testCase "does not match weekly on another day at midnight" $
      scheduleMatches weekly (UTCTime (fromGregorian 2014 6 5) 600) @?= False

    , testCase "only needs weekday or monthday to match" $
      scheduleMatches stars { dayOfWeek = mkDayOfWeekSpec' (Field (SpecificField' (mkSpecificField' 1))),
                              dayOfMonth = mkDayOfMonthSpec' (Field (SpecificField' (mkSpecificField' 1))) }
                      (UTCTime (fromGregorian 2014 11 1) 600) @?= True
    -- https://github.com/MichaelXavier/cron/issues/18
    , testCase "correctly schedules steps and ranges" $ do
      let Right oddMinute = parseOnly cronSchedule "1-59/2 * * * *"
      let Right evenMinute = parseOnly cronSchedule "0-59/2 * * * *"
      let t1 = mkTime 2015 7 17 15 17 0
      let t2 = mkTime 2015 7 17 15 18 0
      scheduleMatches oddMinute t1 @?= True
      scheduleMatches oddMinute t2 @?= False
      scheduleMatches evenMinute t1 @?= False
      scheduleMatches evenMinute t2 @?= True

    , testProperty "star matches everything" $ \t ->
            scheduleMatches stars t

    , testProperty "exact time matches" $ \t ->
      let (_, m, d, h, mn) = timeComponents t
          sched = CronSchedule (mkMinuteSpec' (Field (SpecificField' (mkSpecificField' mn))))
                               (mkHourSpec' (Field (SpecificField' (mkSpecificField' h))))
                               (mkDayOfMonthSpec' (Field (SpecificField' (mkSpecificField' d))))
                               (mkMonthSpec' (Field (SpecificField' (mkSpecificField' m))))
                               (mkDayOfWeekSpec' (Field Star))
      in scheduleMatches sched t

    , testProperty "any time with the same minute as n * * * * matches" $ arbitraryTimeFields $ \y m d h mn ->
      let sched = stars { minute = mkMinuteSpec' (Field (SpecificField' (mkSpecificField' mn))) }
          t     = day' y m d h mn
      in scheduleMatches sched t

    , testProperty "any time with the diff minute as n * * * * does not match" $ arbitraryTimeFields $ \y m d h mn ->
      let sched = stars { minute = mkMinuteSpec' (Field (SpecificField' (mkSpecificField' (stepMax 59 mn)))) }
          t     = day' y m d h mn
      in not $ scheduleMatches sched t

    , testProperty "any time with the same hour as * n * * * matches" $ arbitraryTimeFields $ \y m d h mn ->
      let sched = stars { hour = mkHourSpec' (Field (SpecificField' (mkSpecificField' h))) }
          t     = day' y m d h mn
      in scheduleMatches sched t

    , testProperty "any time with the diff hour as * n * * * does not match" $ arbitraryTimeFields $ \y m d h mn ->
      let sched = stars { hour = mkHourSpec' (Field (SpecificField' (mkSpecificField' (stepMax 23 h)))) }
          t     = day' y m d h mn
      in not $ scheduleMatches sched t

    , testProperty "any time with the same day as * * n * * matches" $ \t ->
      let (_, m, d, h, mn) = timeComponents t
          sched = CronSchedule (mkMinuteSpec' (Field (SpecificField' (mkSpecificField' mn))))
                               (mkHourSpec' (Field (SpecificField' (mkSpecificField' h))))
                               (mkDayOfMonthSpec' (Field (SpecificField' (mkSpecificField' d))))
                               (mkMonthSpec' (Field (SpecificField' (mkSpecificField' m))))
                               (mkDayOfWeekSpec' (Field Star))
      in scheduleMatches sched t

    , testProperty "any time with the diff day as * * n * * does not match" $ arbitraryTimeFields $ \y m d h mn ->
      let sched = stars { dayOfMonth = mkDayOfMonthSpec' (Field (SpecificField' (mkSpecificField' (stepMax 31 d)))) }
          t     = day' y m d h mn
      in not $ scheduleMatches sched t

  ]

  where day = day' 2012
        day' y m d h mn = UTCTime (fromGregorian y m d) (diffTime h mn)
        diffTime h mn = timeOfDayToTime $ TimeOfDay h mn 1

arbitraryTimeFields
    :: (Num r
       , Num r1
       , Num r2
       , Num r3
       , Ord r
       , Ord r1
       , Ord r2
       , Ord r3
       )
    => (a -> r -> r1 -> r2 -> r3 -> t)
    -> Positive a
    -> Positive r
    -> Positive r1
    -> Positive r2
    -> Positive r3
    -> t
arbitraryTimeFields f y m d h mn = f (getPositive y)
                                     (min 12 $ getPositive m)
                                     (min 28 $ getPositive d)
                                     (min 23 $ getPositive h)
                                     (min 59 $ getPositive mn)

hoursMins :: DiffTime -> (Int, Int)
hoursMins uTime = (hr, mn)
  where
    TimeOfDay { todHour = hr,
                todMin  = mn} = timeToTimeOfDay uTime


stepMax :: (Enum a, Ord a) => a -> a -> a
stepMax mx n | n < mx    = succ n
             | otherwise = pred n


describeCronScheduleShow :: TestTree
describeCronScheduleShow = testGroup "CronSchedule show"
  [
    testCase "formats stars" $
    show stars @?= "CronSchedule * * * * *"

  , testCase "formats specific numbers" $
    show stars { dayOfWeek = mkDayOfWeekSpec' (Field (SpecificField' (mkSpecificField' 3)))} @?=
         "CronSchedule * * * * 3"

  , testCase "formats lists" $
    show stars { minute = mkMinuteSpec' (ListField (SpecificField' (mkSpecificField' 1) :| [SpecificField' (mkSpecificField' 2), SpecificField' (mkSpecificField' 3)]))} @?=
        "CronSchedule 1,2,3 * * * *"

  , testCase "formats ranges" $
    show stars { hour = mkHourSpec' (Field (RangeField' (mkRangeField' 7 10)))} @?=
         "CronSchedule * 7-10 * * *"

  , testCase "formats steps" $
    show stars { dayOfMonth = mkDayOfMonthSpec' (StepField' (mkStepField' Star 2))} @?=
        "CronSchedule * * */2 * *"

  , testCase "formats @yearly" $
    show yearly @?= "CronSchedule 0 0 1 1 *"

  , testCase "formats @monthly" $
    show monthly @?= "CronSchedule 0 0 1 * *"

  , testCase "formats @weekly" $
    show weekly @?= "CronSchedule 0 0 * * 0"

  , testCase "formats @daily" $
    show daily @?= "CronSchedule 0 0 * * *"

  , testCase "formats @hourly" $
    show hourly @?= "CronSchedule 0 * * * *"

  , testCase "formats everyMinute" $
    show everyMinute @?= "CronSchedule * * * * *"
  ]

describeCrontabShow :: TestTree
describeCrontabShow = testGroup "Crontab Show"
  [
    testCase "prints nothing for an empty crontab" $
    show (Crontab []) @?= ""
  ]

describeCrontabEntryShow :: TestTree
describeCrontabEntryShow = testGroup "CrontabEntry Show"
  [
   testCase "formats environment variable sets" $
   show envSet @?= "FOO=BAR"

  , testCase "formats command entries" $
    show entry @?= "* * * * * do stuff"
  ]


--TODO: evidently 0/0 0-0 0-0 */0 0-0 may not be valid
describeNextMatch :: TestTree
describeNextMatch = testGroup "nextMatch"
  [ testProperty "is always in the future" $ \cs t ->
      let t2 = traceShow cs $ nextMatch cs t
      in t2 > t
  , testProperty "always produces a time that will match the schedule" $ \cs t ->
      let t2 = nextMatch cs t
      in scheduleMatches cs t2
  -- , testProperty "returns the first minute in the future that matches" $ \cs t ->
  --     let expected = head (filter (scheduleMatches cs) (take 1000 $ nextMinutes t))
  --     in nextMatch cs t === expected
  ]


-- nextMinutes :: UTCTime -> [UTCTime]
-- nextMinutes t = [ addMinutes tRounded mins | mins <- [1..]]
--   where
--     addMinutes time mins = addUTCTime (fromInteger (60 * mins)) time
--     -- round down to nearest 60
--     tRounded = t { utctDayTime = roundToMinute (utctDayTime t)}



-- roundToMinute :: DiffTime -> DiffTime
-- roundToMinute n = secondsToDiffTime (nInt - (nInt `mod` 60))
--   where
--     nInt = round n


envSet :: CrontabEntry
envSet = EnvVariable "FOO" "BAR"

entry :: CrontabEntry
entry = CommandEntry stars "do stuff"

stars :: CronSchedule
stars = CronSchedule (mkMinuteSpec' (Field Star))
                     (mkHourSpec' (Field Star))
                     (mkDayOfMonthSpec' (Field Star))
                     (mkMonthSpec' (Field Star))
                     (mkDayOfWeekSpec' (Field Star))

timeComponents :: UTCTime -> (Integer, Int, Int, Int, Int)
timeComponents (UTCTime dy dt) = (y, m, d, h, mn)
  where
    (y, m, d) = toGregorian dy
    (h, mn)   = hoursMins dt


mkTime
    :: Integer
    -> Int
    -> Int
    -> DiffTime
    -> DiffTime
    -> DiffTime
    -> UTCTime
mkTime y m d hr mn s = UTCTime day time
  where day = fromGregorian y m d
        time = s + 60 * mn + 60 * 60 * hr
