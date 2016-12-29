{-# LANGUAGE OverloadedStrings #-}

module System.Test.Cron.Display (tests) where

-------------------------------------------------------------------------------
import           Data.List.NonEmpty (NonEmpty (..))
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
          "Every minute, every hour, every day, every month, every day of the week" @=? describe (mkCronSchedule "* * * * *")
        , testCase "non-verbose" $
          "Every minute" @=? describe (mkCronSchedule "* * * * *")
    ]
    , testGroup "displays specific values" [
        testCase "verbose" $
          "" @=? describe (mkCronSchedule "1 2 3 * *")
    ]
    , testGroup "displays list values" [
        testCase "verbose" $
          "" @=? describe (mkCronSchedule "* * 3,5 * *")
    ]
    , testGroup "displays range values" [
        testCase "verbose" $
          "" @=? describe (mkCronSchedule "* * 3-4 * *")
    ]
    , testGroup "displays step values" [
        testCase "verbose" $
          "" @=? describe (mkCronSchedule "*/2 * 2-10/4 * *")
    ]
    , testGroup "displays other values" [
        testCase "verbose" $
          "" @=? describe (mkCronSchedule "1-59/2 * * 2 3-5")
    ]
  ]
  where
    mkCronSchedule t = let (Right cs) = parseCronSchedule t in cs


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
        "at 02:01, 03:01, at 1 minutes past the hour between 04:00 and 11:00" @=?
        describeTime (mkMinuteSpec' (Field (SpecificField' (mkSpecificField' 1))))
                     (mkHourSpec'   (ListField (SpecificField' (mkSpecificField' 2) :| [SpecificField' (mkSpecificField' 3), RangeField' (mkRangeField' 4 11) ] )))
    , testCase "list of hours, and star" $
        "at 1 minutes past the hour, every hour" @=? describeTime (mkMinuteSpec' (Field (SpecificField' (mkSpecificField' 1))))
                                                                  (mkHourSpec'   (ListField (SpecificField' (mkSpecificField' 2) :| [Star])))
    ]
  , testGroup "displays other times" [
      testCase "every minute, every hour" $
        "every minute, every hour" @=? describeTime (mkMinuteSpec' (Field Star))
                                                    (mkHourSpec'   (Field Star))
    , testCase "range of minutes, range of hours" $
        "minutes 10 through 15 past the hour, between 01:00 and 03:00" @=?
        describeTime (mkMinuteSpec' (Field (RangeField' (mkRangeField' 10 15))))
                     (mkHourSpec'   (Field (RangeField' (mkRangeField' 1 3))))
    , testCase "range of minutes, every hour" $
        "minutes 10 through 15 past the hour, every hour" @=?
        describeTime (mkMinuteSpec' (Field (RangeField' (mkRangeField' 10 15))))
                     (mkHourSpec'   (Field Star))
    , testCase "range of minutes, interval of hours" $
        "minutes 10 through 15 past the hour, every 3 hours, starting at 03:00" @=?
        describeTime (mkMinuteSpec' (Field (RangeField' (mkRangeField' 10 15))))
                     (mkHourSpec'   (StepField' (mkStepField' (SpecificField' (mkSpecificField' 3)) 3)))
    , testCase "list of minutes, at an hour" $
        "every minute, at 03:00" @=?
        describeTime (mkMinuteSpec' (ListField (SpecificField' (mkSpecificField' 2) :| [Star])))
                     (mkHourSpec'   (Field (SpecificField' (mkSpecificField' 3))))
    , testCase "step minutes, step hours" $
        "every 3 minutes, minutes 10 through 15 past the hour, every 5 hours, starting at 10:00" @=?
        describeTime (mkMinuteSpec' (StepField' (mkStepField' (RangeField' (mkRangeField' 10 15)) 3)))
                     (mkHourSpec'   (StepField' (mkStepField' (SpecificField' (mkSpecificField' 10)) 5)))
    ]
  ]
