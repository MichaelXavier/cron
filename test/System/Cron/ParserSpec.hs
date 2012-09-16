{-# LANGUAGE OverloadedStrings #-}
module System.Cron.ParserSpec (spec) where
-- TODO: this *should* just work with {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Data.Attoparsec.Text (parseOnly, Parser)
import Data.Text (Text)
import Test.Hspec

import System.Cron
import System.Cron.Parser

spec :: Spec
spec = sequence_ [describeCronSchedule,
                  describeCronScheduleLoose,
                  describeCrontab,
                  describeCrontabEntry]

describeCronSchedule :: Spec
describeCronSchedule = describe "cronSchedule" $ do
  it "parses all stars" $
    assertSuccessfulParse "* * * * *"
                          stars

  it "parses specific values" $
    assertSuccessfulParse "1 2 3 * *"
                           stars { minute      = Minutes (SpecificField 1),
                                   hour        = Hours (SpecificField 2),
                                   dayOfMonth  = DaysOfMonth (SpecificField 3) }

  it "parses list values" $
    assertSuccessfulParse "* * 3,4 * *"
                           stars { dayOfMonth  = DaysOfMonth (ListField [SpecificField 3,
                                                                         SpecificField 4]) }

  it "parses range values" $
    assertSuccessfulParse "* * 3-4 * *"
                           stars { dayOfMonth  = DaysOfMonth (RangeField 3 4) }

  it "parses step values" $
    assertSuccessfulParse "*/2 * 2-10/4 * *"
                          stars { minute     = Minutes (StepField Star 2),
                                  dayOfMonth  = DaysOfMonth (StepField (RangeField 2 10) 4) }

  it "refuses to parse recursive steps" $
    assertFailedParse "*/2/3 * * * *"

  it "refuses to parse sparse lists" $
    assertFailedParse "1,,2 * * * *"

  it "refuses too few fields" $
    assertFailedParse "* * * *"

  it "refuses too many fields" $
    assertFailedParse "* * * * * *"

  it "refuses extraneous input" $
    assertFailedParse "* * * * *    wat is this"

  it "parses @hourly" $
    assertSuccessfulParse "@hourly" hourly

  it "parses @daily" $
    assertSuccessfulParse "@daily" daily

  it "parses @monthly" $
    assertSuccessfulParse "@monthly" monthly

  it "parses @yearly" $
    assertSuccessfulParse "@yearly" yearly

  it "parses ranges at the last field" $
    assertSuccessfulParse "* * * * 3-4"
                           stars { dayOfWeek  = DaysOfWeek (RangeField 3 4) }
  it "parses lists at the last field" $
    assertSuccessfulParse "* * * * 3,4"
                           stars { dayOfWeek  = DaysOfWeek (ListField [SpecificField 3,
                                                                       SpecificField 4]) }
  it "parses steps at the last field" $
    assertSuccessfulParse "* * * * */4"
                           stars { dayOfWeek  = DaysOfWeek (StepField Star 4) }
  where assertSuccessfulParse = assertParse cronSchedule
        assertFailedParse = assertNoParse cronSchedule 

describeCronScheduleLoose :: Spec
describeCronScheduleLoose = describe "cronScheduleLoose" $ do
  it "is okay with extaneous input" $
    assertSuccessfulParse "* * * * * *"
                          stars
  where assertSuccessfulParse = assertParse cronScheduleLoose

describeCrontab :: Spec
describeCrontab = describe "crontab" $ do
  it "parses an empty input" $
    assertSuccessfulParse ""
                          (Crontab [])

  it "parses whitespace" $
    assertSuccessfulParse "        "
                          (Crontab [])

  it "parses a single line" $
    assertSuccessfulParse "        "
                          (Crontab [])

  it "ignores comments" $
    assertSuccessfulParse "# comment"
                          (Crontab [])

  it "ignores comments with leading whitespace" $
    assertSuccessfulParse "    # comment"
                          (Crontab [])

  it "parses comments interspersed with actual commands" $
    assertSuccessfulParse "#comment here\nFOO=BAR\n  #another\n* * * * * do stuff"
                          (Crontab [envSet, entry])

  where assertSuccessfulParse = assertParse crontab

describeCrontabEntry :: Spec
describeCrontabEntry = describe "crontabEntry" $ do
  it "parses an environment variable assignment" $ 
    assertSuccessfulParse "FOO=BAR"
                          envSet

  it "pparses an environment variable with whitespace at the front" $ 
    assertSuccessfulParse "  FOO=BAR"
                          envSet

  it "pparses an environment variable with whitespace in the middle" $ 
    assertSuccessfulParse "  FOO =   BAR"
                          envSet

  it "parses a command" $ 
    assertSuccessfulParse "* * * * * do stuff"
                          entry

  it "parses a command with any amount of whitespace inbetween" $ 
    assertSuccessfulParse "* * * * *      do stuff"
                          entry

  it "pparses a command with whitespace at the front" $ 
    assertSuccessfulParse "  * * * * *      do stuff"
                          entry
  where assertSuccessfulParse = assertParse crontabEntry

assertParse :: (Eq a, Show a)
               => Parser a
               -> Text
               -> a
               -> Expectation
assertParse parser txt expected = parsed `shouldBe` Right expected
  where parsed = parseOnly parser txt

--assertNoParse :: Parser a -> Text -> b
assertNoParse :: (Eq a, Show a)
                 => Parser a
                 -> Text
                 -> Expectation
assertNoParse parser txt = isLeft parsed `shouldBe` True
  where isLeft (Left _) = True
        isLeft _        = False
        parsed          = parseOnly parser txt

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
