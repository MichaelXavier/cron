module System.Test.Cron.Display (tests) where

-------------------------------------------------------------------------------
import           Data.List.NonEmpty (NonEmpty (..))
-- import           Data.Text          (Text)
-------------------------------------------------------------------------------
import           SpecHelper
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "System.Cron.Display"
  [ describeDisplayCronSchedule
  , describeDisplayTime
  ]


-------------------------------------------------------------------------------
describeDisplayCronSchedule :: TestTree
describeDisplayCronSchedule = testGroup "displayCronSchedule"
  [
      testGroup "displays all stars" [
        testCase "verbose" $
          "Every minute, every hour, every day, every month, every weekday" @=? displaySchedule Verbose stars
        , testCase "non-verbose" $
          "Every minute" @=? displaySchedule NonVerbose stars
    ]
    , testGroup "displays specific values" [
        testCase "verbose" $
          "" @=? displaySchedule NonVerbose specificValue
    ]
    , testGroup "displays list values" [
        testCase "verbose" $
          "" @=? displaySchedule NonVerbose listValue
    ]
    , testGroup "displays range values" [
        testCase "verbose" $
          "" @=? displaySchedule NonVerbose rangeValue
    ]
    , testGroup "displays step values" [
        testCase "verbose" $
          "" @=? displaySchedule NonVerbose stepValue
    ]
    , testGroup "displays other values" [
        testCase "verbose" $
          "" @=? displaySchedule NonVerbose combo
    ]
  ]
  where
    stars = CronSchedule (mkMinuteSpec' (Field Star))
                         (mkHourSpec' (Field Star))
                         (mkDayOfMonthSpec' (Field Star))
                         (mkMonthSpec' (Field Star))
                         (mkDayOfWeekSpec' (Field Star))

    specificValue = stars { minute      = mkMinuteSpec' (Field (SpecificField' (mkSpecificField' 1))),
                            hour        = mkHourSpec' (Field (SpecificField' (mkSpecificField' 2))),
                            dayOfMonth  = mkDayOfMonthSpec' (Field (SpecificField' (mkSpecificField' 3))) }

    listValue = stars { dayOfMonth  = mkDayOfMonthSpec' (ListField (SpecificField' (mkSpecificField' 3) :| [SpecificField' (mkSpecificField' 4)])) }

    rangeValue = stars { dayOfMonth  = mkDayOfMonthSpec' (Field (RangeField' (mkRangeField' 3 4))) }

    stepValue = stars { minute      = mkMinuteSpec' (StepField' (mkStepField' Star 2)),
                        dayOfMonth  = mkDayOfMonthSpec' (StepField' (mkStepField' (RangeField' (mkRangeField' 2 10)) 4)) }

    combo = stars { minute     = mkMinuteSpec' (StepField' (mkStepField' (RangeField' (mkRangeField' 1 59)) 2)) }


-------------------------------------------------------------------------------
describeDisplayTime :: TestTree
describeDisplayTime = testGroup "describeDisplayTime"
  [
    testCase "displays specific times" $
      "at 02:01" @=? describeTime (mkMinuteSpec' (Field (SpecificField' (mkSpecificField' 1))))
                                  (mkHourSpec'   (Field (SpecificField' (mkSpecificField' 2))))
  , testCase "displays a range of minutes" $
      "every minute between 02:01 and 02:10" @=? describeTime (mkMinuteSpec' (Field (RangeField' (mkRangeField' 1 10))))
                                                              (mkHourSpec'   (Field (SpecificField' (mkSpecificField' 2))))
  , testGroup "displays times for lists of hours" [
      testCase "simple list of hours" $
        "at 02:01, 03:01" @=? describeTime (mkMinuteSpec' (Field (SpecificField' (mkSpecificField' 1))))
                                           (mkHourSpec'   (ListField (SpecificField' (mkSpecificField' 2) :| [SpecificField' (mkSpecificField' 3)])))
    , testCase "list of hours, and range" $
        "at 02:01, 03:01, 1 minutes past the hour, between 04 and 11" @=?
        describeTime (mkMinuteSpec' (Field (SpecificField' (mkSpecificField' 1))))
                     (mkHourSpec'   (ListField (SpecificField' (mkSpecificField' 2) :| [SpecificField' (mkSpecificField' 3), RangeField' (mkRangeField' 4 11) ] )))
    , testCase "list of hours, and star" $
        "at 1 minutes past the hour, every hour" @=? describeTime (mkMinuteSpec' (Field (SpecificField' (mkSpecificField' 1))))
                                                                  (mkHourSpec'   (ListField (SpecificField' (mkSpecificField' 2) :| [Star])))
    ]
  ]
