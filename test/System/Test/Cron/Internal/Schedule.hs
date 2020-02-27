module System.Test.Cron.Internal.Schedule
    ( tests
    ) where

-------------------------------------------------------------------------------
import           Hedgehog
-------------------------------------------------------------------------------
import           SpecHelper
import           System.Cron.Internal.Schedule
-------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "System.Cron.Schedule"
  [ describeFindNextMinuteDelay'
  ]

describeFindNextMinuteDelay' :: TestTree
describeFindNextMinuteDelay' = testGroup "findNextMinuteDelay'"
  [
    testCase "should find next minute of arbitrary time." $
      let now = UTCTime { utctDay = ModifiedJulianDay 58906, utctDayTime = picosecondsToDiffTime 29085749437046123} -- 2020-02-27 08:04:45.749437046 UTC
          nextMinute = UTCTime { utctDay = ModifiedJulianDay 58906, utctDayTime = picosecondsToDiffTime 29100000000000000} -- 2020-02-27 08:05:00.000000000 UTC
       in findNextMinuteDelay' now @?= (nextMinute, 14250563)
    , testCase "should find next minute of 59 minutes." $
      let now = UTCTime { utctDay = ModifiedJulianDay 58906, utctDayTime = picosecondsToDiffTime 32357749437046123} -- 2020-02-27 08:59:17.749437046123 UTC
          nextMinute = UTCTime { utctDay = ModifiedJulianDay 58906, utctDayTime = picosecondsToDiffTime 32400000000000000} -- 2020-02-27 08:05:00.000000000 UTC
       in findNextMinuteDelay' now @?= (nextMinute, 42250563)
    , testCase "should find next minute of 0 minute." $
      let now = UTCTime { utctDay = ModifiedJulianDay 58906, utctDayTime = picosecondsToDiffTime 32400123456789012} -- 2020-02-27 09:00:00.123456789012 UTC
          nextMinute = UTCTime { utctDay = ModifiedJulianDay 58906, utctDayTime = picosecondsToDiffTime 32460000000000000} -- 2020-02-27 08:05:00.000000000 UTC
       in findNextMinuteDelay' now @?= (nextMinute, 59876543)
    , testProperty "invariance: next minute is after and no more than 60s later." $ property $ do
        now <- forAll gen
        let (nextMinute, delay) = findNextMinuteDelay' now
        ((nextMinute > now) && (delay <= 60000000)) === True
    , testProperty "invariance: next minute after next minute is *exactly* 60s." $ property $ do
        now <- forAll gen
        let (nextMinute1, _) = findNextMinuteDelay' now
        let (nextMinute2, delay) = findNextMinuteDelay' nextMinute1
        ((diffUTCTime nextMinute2 nextMinute1 == 60) && (delay == 60000000)) === True
  ]
