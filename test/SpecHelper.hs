{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module SpecHelper
    ( module X
    , module SpecHelper
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative       as X
import           Data.Attoparsec.Text      as X (Parser, parseOnly)
import           Data.DeriveTH
import           Data.List.NonEmpty        (NonEmpty (..))
import           Data.Maybe                as X
import           Data.Monoid               as X
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time.Calendar        as X
import           Data.Time.Clock           as X
import           Data.Time.LocalTime       as X
import           Debug.Trace               as X
import           Test.QuickCheck.Instances ()
import           Test.Tasty                as X
import           Test.Tasty.HUnit          as X
import           Test.Tasty.QuickCheck     as X
-------------------------------------------------------------------------------
import           System.Cron               as X
-------------------------------------------------------------------------------


$(derive makeArbitrary ''NonEmpty)
$(derive makeArbitrary ''BaseField)
$(derive makeArbitrary ''CronField)
$(derive makeArbitrary ''CronSchedule)


instance Arbitrary Crontab where
  arbitrary = Crontab <$> resize 20 arbitrary


instance Arbitrary CronCommand where
  arbitrary = CronCommand <$> alphaGen


instance Arbitrary CrontabEntry where
  arbitrary = oneof [ CommandEntry <$> arbitrary <*> arbitrary
                    , EnvVariable <$> alphaGen <*> alphaGen
                    ]


alphaGen :: Gen Text
alphaGen = T.pack <$> listOf1 gen
  where
    gen = elements (['a'..'z'] <> ['A'..'Z'])

instance Arbitrary MinuteSpec where
  arbitrary = arbitraryMaybe mkMinuteSpec


instance Arbitrary HourSpec where
  arbitrary = arbitraryMaybe mkHourSpec


instance Arbitrary DayOfMonthSpec where
  arbitrary = arbitraryMaybe mkDayOfMonthSpec


instance Arbitrary MonthSpec where
  arbitrary = arbitraryMaybe mkMonthSpec


instance Arbitrary DayOfWeekSpec where
  arbitrary = arbitraryMaybe mkDayOfWeekSpec


instance Arbitrary SpecificField where
  arbitrary = arbitraryMaybe mkSpecificField


instance Arbitrary RangeField where
  arbitrary = arbitraryMaybe (uncurry mkRangeField)


instance Arbitrary StepField where
  arbitrary = arbitraryMaybe (uncurry mkStepField)


arbitraryMaybe :: Arbitrary a => (a -> Maybe b) -> Gen b
arbitraryMaybe f = do
  a <- arbitrary `suchThat` (isJust . f)
  return (fromJust (f a))


mkMinuteSpec' :: CronField -> MinuteSpec
mkMinuteSpec' = fromJust . mkMinuteSpec


mkHourSpec' :: CronField -> HourSpec
mkHourSpec' = fromJust . mkHourSpec


mkDayOfMonthSpec' :: CronField -> DayOfMonthSpec
mkDayOfMonthSpec' = fromJust . mkDayOfMonthSpec


mkMonthSpec' :: CronField -> MonthSpec
mkMonthSpec' = fromJust . mkMonthSpec


mkDayOfWeekSpec' :: CronField -> DayOfWeekSpec
mkDayOfWeekSpec' = fromJust . mkDayOfWeekSpec


mkRangeField' :: Int -> Int -> RangeField
mkRangeField' a = fromJust . mkRangeField a


mkSpecificField' :: Int -> SpecificField
mkSpecificField' = fromJust . mkSpecificField


mkStepField' :: BaseField -> Int -> StepField
mkStepField' a = fromJust . mkStepField a


-------------------------------------------------------------------------------
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
