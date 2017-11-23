{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell       #-}
module SpecHelper
    ( module X
    , module SpecHelper
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative       as X
import           Data.Attoparsec.Text      as X (Parser, parseOnly)
import qualified Data.List.NonEmpty        as NE
import           Data.Maybe                as X
import           Data.Monoid               as X
import           Data.Proxy                (Proxy (..))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time.Calendar        as X
import           Data.Time.Clock           as X
import           Data.Time.LocalTime       as X
import           Debug.Trace               as X
import qualified Generics.SOP              as SOP
import qualified Generics.SOP.Constraint   as SOP
import qualified Generics.SOP.GGP          as SOP
import           GHC.Generics              (Generic)
import           Test.QuickCheck.Instances ()
import           Test.Tasty                as X
import           Test.Tasty.HUnit          as X
import           Test.Tasty.QuickCheck     as X
-------------------------------------------------------------------------------
import           System.Cron               as X
-------------------------------------------------------------------------------


-- this workaround is in place until we successfully beat down the
-- doors of castle QuickCheck and get generic deriving through. See
-- <https://github.com/nick8325/quickcheck/pull/40>
sopArbitrary :: (SOP.GTo b, SOP.SListI (SOP.GCode b), Generic b, SOP.AllF (SOP.All Arbitrary) (SOP.GCode b), SOP.AllF SOP.SListI (SOP.GCode b)) => Gen b
sopArbitrary = fmap SOP.gto sopArbitrary'


sopArbitrary' :: (SOP.SListI xss, SOP.AllF SOP.SListI xss, SOP.AllF (SOP.All Arbitrary) xss) => Gen (SOP.SOP SOP.I xss)
sopArbitrary' = oneof (map SOP.hsequence $ SOP.apInjs_POP $ SOP.hcpure p arbitrary)
  where
    p :: Proxy Arbitrary
    p = Proxy


instance Arbitrary BaseField where
  arbitrary = sopArbitrary
  shrink = genericShrink


instance Arbitrary CronField where
  arbitrary = oneof [ Field <$> arbitrary
                    , ListField . NE.fromList . getNonEmpty <$> arbitrary
                    , StepField' <$> arbitrary
                    ]


instance Arbitrary CronSchedule where
  arbitrary = sopArbitrary
  shrink = genericShrink


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
