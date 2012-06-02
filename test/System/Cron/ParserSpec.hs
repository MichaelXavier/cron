{-# LANGUAGE OverloadedStrings #-}
module System.Cron.ParserSpec (spec) where
-- TODO: this *should* just work with {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Data.Attoparsec.Text (parseOnly)
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit.Base ((~?=))

import System.Cron
import System.Cron.Parser

spec :: Spec
spec = sequence_ [describeCronSchedule]

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
  where assertSuccessfulParse txt expected = doParse txt ~?= Right expected
        assertFailedParse txt = isLeft (doParse txt) ~?= True
        isLeft (Left _)       = True
        isLeft _              = False
        doParse = parseOnly cronSchedule

stars :: CronSchedule
stars = CronSchedule (Minutes Star)
                     (Hours Star)
                     (DaysOfMonth Star)
                     (Months Star)
                     (DaysOfWeek Star)
