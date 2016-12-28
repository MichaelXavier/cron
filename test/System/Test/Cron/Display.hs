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
  ]


-------------------------------------------------------------------------------
describeDisplayCronSchedule :: TestTree
describeDisplayCronSchedule = testGroup "displayCronSchedule"
  [
      testGroup "displays all stars" [
        testCase "verbose" $
          "Every minute, every hour, every day" @=? displaySchedule Verbose stars
        , testCase "non-verbose" $
          "Every minute" @=? displaySchedule NonVerbose stars
    ]
    , testGroup "displays specific values" [
        testCase "verbose" $
          "" @=? displaySchedule Verbose specificValue
    ]
    , testGroup "displays list values" [
        testCase "verbose" $
          "" @=? displaySchedule Verbose listValue
    ]
    , testGroup "displays range values" [
        testCase "verbose" $
          "" @=? displaySchedule Verbose rangeValue
    ]
    , testGroup "displays step values" [
        testCase "verbose" $
          "" @=? displaySchedule Verbose stepValue
    ]
    , testGroup "displays other values" [
        testCase "verbose" $
          "" @=? displaySchedule Verbose combo
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
