{-# LANGUAGE OverloadedStrings #-}

module System.Test.Cron.Describe (tests) where

import           SpecHelper
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "System.Cron.Describe"
  [ describeDisplayCronSchedule
  ]


-------------------------------------------------------------------------------
describeDisplayCronSchedule :: TestTree
describeDisplayCronSchedule = testGroup "describeCronSchedule"
  [
      testGroup "describes all stars" [
        testCase "verbose" $
          "Every minute, every hour, every day, every day of the week" @=? describeV "* * * * *"

      , testCase "non-verbose" $
          "Every minute" @=? describeNV "* * * * *"
    ]
    , testGroup "describes specific values" [
        testCase "verbose" $
          "At 02:01, on day 3 of the month, every day of the week" @=? describeV "1 2 3 * *"

      , testCase "non-verbose" $
          "At 02:01, on day 3 of the month" @=? describeNV "1 2 3 * *"

      , testCase "12-hour (verbose)" $
          "At 02:01 AM, on day 3 of the month, every day of the week" @=? describe12 "1 2 3 * *"
    ]
    , testGroup "describes list values" [
        testCase "verbose" $
          "Every minute, every hour, on days 3 and 5 of the month, every day of the week" @=? describeV "* * 3,5 * *"

      , testCase "non-verbose" $
          "Every minute, on days 3 and 5 of the month" @=? describeNV "* * 3,5 * *"
    ]
    , testGroup "describes range values" [
        testCase "verbose" $
          "Every minute, every hour, between days 3 and 4 of the month, every day of the week" @=? describeV "* * 3-4 * *"

      , testCase "non-verbose" $
          "Every minute, between days 3 and 4 of the month" @=? describeNV "* * 3-4 * *"
    ]
    , testGroup "describes step values" [
        testCase "verbose" $
          "Every 2 minutes, every hour, every 4 days, between days 2 and 10 of the month, every day of the week" @=? describeV "*/2 * 2-10/4 * *"

      , testCase "non-verbose" $
          "Every 2 minutes, every 4 days, between days 2 and 10 of the month" @=? describeNV "*/2 * 2-10/4 * *"
    ]
    , testGroup "describes other values" [
        testCase "verbose" $
          "Every 2 minutes, minutes 1 through 59 past the hour, every hour, every day, Wednesday through Friday, only in February" @=? describeV "1-59/2 * * 2 3-5"

      , testCase "non-verbose" $
          "Every 2 minutes, minutes 1 through 59 past the hour, Wednesday through Friday, only in February" @=? describeNV "1-59/2 * * 2 3-5"
    ]
    , testGroup "handles Sunday weirdness" [
        testCase "range of values (Sunday at 0)" $
          "Every minute, every hour, every day, Sunday through Friday" @=? describeV "* * * * 0-5"

      , testCase "range of values (Sunday at 7)" $
          "Every minute, every hour, every day, Friday through Sunday" @=? describeV "* * * * 5-7"

      , testCase "range of values" $
          "Every minute, every hour, every day, Sunday through Sunday" @=? describeV "* * * * 0-7"
    ]
    , testGroup "describes complicated times" [
        testCase "describes specific times - twenty four hour" $
          "At 13:01, every day, every day of the week" @=? describeV "1 13 * * *"

      , testCase "describes specific times - twelve hour" $
        "At 01:01 PM, every day, every day of the week" @=? describe12 "1 13 * * *"

      , testCase "describes a range of minutes - twenty four hour" $
          "Every minute between 12:01 and 12:10, every day, every day of the week" @=? describeV "1-10 12 * * *"

      , testCase "describes a range of minutes - twelve hour" $
          "Every minute between 12:01 PM and 12:10 PM, every day, every day of the week" @=? describe12 "1-10 12 * * *"

      , testGroup "describes times for lists of hours" [
          testCase "simple list of hours - twenty four hour" $
            "At 02:01 and 03:01, every day, every day of the week" @=? describeV "1 2,3 * * *"

        , testCase "simple list of hours - twelve hour" $
          "At 02:01 AM and 03:01 AM, every day, every day of the week" @=? describe12 "1 2,3 * * *"

        , testCase "list of hours, and range - twenty four hour" $
            "At 02:01, 03:01 and at 1 minutes past the hour between 04:00 and 13:00, every day, every day of the week" @=?
            describeV "1 2,3,4-13 * * *"

        , testCase "list of hours, and range - twelve hour" $
            "At 02:01 AM, 03:01 AM and at 1 minutes past the hour between 04:00 AM and 01:00 PM, every day, every day of the week" @=?
            describe12 "1 2,3,4-13 * * *"

        , testCase "list of hours, and star" $
            "At 1 minutes past the hour, every hour, every day, every day of the week" @=? describeV "1 2,* * * *"
      ]
    , testGroup "describes other times" [
        testCase "range of minutes, range of hours" $
          "Minutes 10 through 15 past the hour, between 01:00 and 03:00, every day, every day of the week" @=?
          describeV "10-15 1-3 * * *"

      , testCase "range of minutes, every hour" $
          "Minutes 10 through 15 past the hour, every hour, every day, every day of the week" @=?
          describeV "10-15 * * * *"

      , testCase "range of minutes, interval of hours" $
          "Minutes 10 through 15 past the hour, every 3 hours, starting at 03:00, every day, every day of the week" @=?
          describeV "10-15 3/3 * * *"

      , testCase "list of minutes, at an hour" $
          "Every minute, at 03:00, every day, every day of the week" @=?
          describeV "2,* 3 * * *"

      , testCase "step minutes, step hours" $
          "Every 3 minutes, minutes 10 through 15 past the hour, every 5 hours, starting at 10:00, every day, every day of the week" @=?
          describeV "10-15/3 10/5 * * *"
      ]
    ]
  ]
  where
    mkCronSchedule t = let (Right cs) = parseCronSchedule t in cs
    describeNV = describe (twentyFourHourFormat <> notVerbose) . mkCronSchedule
    describeV  = describe (twentyFourHourFormat <> verbose) . mkCronSchedule
    describe12 = describe verbose . mkCronSchedule
