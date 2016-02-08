{-# LANGUAGE OverloadedStrings #-}
module System.Test.Cron.Parser (tests) where

-------------------------------------------------------------------------------
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text          (Text)
-------------------------------------------------------------------------------
import           SpecHelper
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "System.Cron.Parser"
  [ describeCronSchedule
  , describeCronScheduleLoose
  , describeCrontab
  , describeCrontabEntry
  ]

describeCronSchedule :: TestTree
describeCronSchedule = testGroup "cronSchedule"
  [
    testCase "parses all stars" $
    assertSuccessfulParse "* * * * *"
                          stars

  , testCase "parses specific values" $
    assertSuccessfulParse "1 2 3 * *"
                           stars { minute      = Minutes (Field (SpecificField 1)),
                                   hour        = Hours (Field (SpecificField 2)),
                                   dayOfMonth  = DaysOfMonth (Field (SpecificField 3)) }

  , testCase "parses list values" $
    assertSuccessfulParse "* * 3,4 * *"
                          stars { dayOfMonth  = DaysOfMonth (ListField (SpecificField 3 :| [SpecificField 4])) }

  , testCase "parses range values" $
    assertSuccessfulParse "* * 3-4 * *"
                           stars { dayOfMonth  = DaysOfMonth (Field (RangeField 3 4)) }

  , testCase "parses step values" $
    assertSuccessfulParse "*/2 * 2-10/4 * *"
                          stars { minute     = Minutes (StepField Star 2),
                                  dayOfMonth  = DaysOfMonth (StepField (RangeField 2 10) 4) }

  , testCase "refuses to parse recursive steps" $
    assertFailedParse "*/2/3 * * * *"

  , testCase "refuses to parse sparse lists" $
    assertFailedParse "1,,2 * * * *"

  , testCase "refuses too few fields" $
    assertFailedParse "* * * *"

  , testCase "refuses too many fields" $
    assertFailedParse "* * * * * *"

  , testCase "refuses extraneous input" $
    assertFailedParse "* * * * *    wat is this"

  , testCase "parses @hourly" $
    assertSuccessfulParse "@hourly" hourly

  , testCase "parses @daily" $
    assertSuccessfulParse "@daily" daily

  , testCase "parses @monthly" $
    assertSuccessfulParse "@monthly" monthly

  , testCase "parses @yearly" $
    assertSuccessfulParse "@yearly" yearly

  , testCase "parses ranges at the last field" $
    assertSuccessfulParse "* * * * 3-4"
                           stars { dayOfWeek  = DaysOfWeek (Field (RangeField 3 4)) }
  , testCase "parses lists at the last field" $
    assertSuccessfulParse "* * * * 3,4"
                           stars { dayOfWeek  = DaysOfWeek (ListField (SpecificField 3 :| [SpecificField 4])) }
  , testCase "parses steps at the last field" $
    assertSuccessfulParse "* * * * */4"
                           stars { dayOfWeek  = DaysOfWeek (StepField Star 4) }
  , testCase "parses a sunday as 7" $
    assertSuccessfulParse "* * * * 7"
                           stars { dayOfWeek  = DaysOfWeek (Field (SpecificField 7)) }
  , testCase "parses a sunday as 0" $
    assertSuccessfulParse "* * * * 0"
                           stars { dayOfWeek  = DaysOfWeek (Field (SpecificField 0)) }
  , testCase "parses another example" $
    assertSuccessfulParse "1-59/2 * * * *"
                          stars { minute     = Minutes (StepField (RangeField 1 59) 2) }

  ]
  where assertSuccessfulParse = assertParse cronSchedule
        assertFailedParse = assertNoParse cronSchedule

describeCronScheduleLoose :: TestTree
describeCronScheduleLoose = testGroup "cronScheduleLoose"
  [
    testCase "is okay with extaneous input" $
    assertSuccessfulParse "* * * * * *"
                          stars
  ]
  where assertSuccessfulParse = assertParse cronScheduleLoose

describeCrontab :: TestTree
describeCrontab = testGroup "crontab"
  [
    testCase "parses an empty input" $
    assertSuccessfulParse ""
                          (Crontab [])

  , testCase "parses whitespace" $
    assertSuccessfulParse "        "
                          (Crontab [])

  , testCase "parses a single line" $
    assertSuccessfulParse "        "
                          (Crontab [])

  , testCase "ignores comments" $
    assertSuccessfulParse "# comment"
                          (Crontab [])

  , testCase "ignores comments with leading whitespace" $
    assertSuccessfulParse "    # comment"
                          (Crontab [])

  , testCase "parses comments interspersed with actual commands" $
    assertSuccessfulParse "#comment here\nFOO=BAR\n  #another\n* * * * * do stuff"
                          (Crontab [envSet, entry])
  ]
  where assertSuccessfulParse = assertParse crontab

describeCrontabEntry :: TestTree
describeCrontabEntry = testGroup "crontabEntry"
  [
    testCase "parses an environment variable assignment" $
    assertSuccessfulParse "FOO=BAR"
                          envSet

  , testCase "parses an environment variable with whitespace at the front" $
    assertSuccessfulParse "  FOO=BAR"
                          envSet

  , testCase "parses an environment variable with whitespace in the middle" $
    assertSuccessfulParse "  FOO =   BAR"
                          envSet

  , testCase "parses a command" $
    assertSuccessfulParse "* * * * * do stuff"
                          entry

  , testCase "parses a command with any amount of whitespace inbetween" $
    assertSuccessfulParse "* * * * *      do stuff"
                          entry

  , testCase "parses a command with whitespace at the front" $
    assertSuccessfulParse "  * * * * *      do stuff"
                          entry
  ]
  where assertSuccessfulParse = assertParse crontabEntry

assertParse :: (Eq a, Show a)
               => Parser a
               -> Text
               -> a
               -> Assertion
assertParse parser txt expected = parsed @?= Right expected
  where parsed = parseOnly parser txt

--assertNoParse :: Parser a -> Text -> b
assertNoParse :: (Eq a, Show a)
                 => Parser a
                 -> Text
                 -> Assertion
assertNoParse parser txt = isLeft (parseOnly parser txt) @?= True

envSet :: CrontabEntry
envSet = EnvVariable "FOO" "BAR"

entry :: CrontabEntry
entry = CommandEntry stars "do stuff"

stars :: CronSchedule
stars = everyMinute
