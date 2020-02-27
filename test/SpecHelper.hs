{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpecHelper
    ( module X
    , module SpecHelper
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative   as X
import           Data.Attoparsec.Text  as X (Parser, parseOnly)
import qualified Data.List.NonEmpty    as NE
import           Data.Maybe            as X
import           Data.Monoid           as X
import           Data.Text             (Text)
import           Data.Time.Calendar    as X (Day (..), fromGregorian,
                                             toGregorian)
import           Data.Time.Clock       as X (DiffTime, UTCTime (..), addUTCTime,
                                             diffUTCTime, picosecondsToDiffTime,
                                             secondsToDiffTime)
import qualified Data.Time.Clock.POSIX as POSIX
import           Data.Time.LocalTime   as X
import           Debug.Trace           as X
import           GHC.Generics          (Generic)
import qualified GHC.Generics          as G
import           Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import           Test.Tasty            as X
import           Test.Tasty.Hedgehog   as X
import           Test.Tasty.HUnit      as X
-------------------------------------------------------------------------------
import           System.Cron           as X
-------------------------------------------------------------------------------


-- | A necessary evil for generators that can generically make sure
-- all constructors of your sum type are covered. The tradeoff is that
-- you have no control over the range at the per-test level, so use
-- this sparingly where full coverage is more valuable than
-- fine-tuning generators.
class HasGen a where
  gen :: Gen a
  default gen :: (Generic a, GHasGen (G.Rep a)) => Gen a
  gen = Gen.choice (fmap G.to <$> ggen)


class GHasGen f where
  ggen :: [Gen (f a)]

instance GHasGen G.U1 where
  ggen = [pure G.U1]

instance (HasGen a) => GHasGen (G.K1 i a) where
  ggen = [G.K1 <$> gen]

instance (GHasGen a, GHasGen b) => GHasGen (a G.:*: b) where
  ggen = [ (G.:*:) <$> f <*> g | f <- ggen, g <- ggen]

instance (GHasGen a, GHasGen b) => GHasGen (a G.:+: b) where
  ggen = (fmap G.L1 <$> ggen) <> (fmap G.R1 <$> ggen)

-- we don't care about metadata, lift over it
instance (GHasGen c) => GHasGen (G.M1 _a _b c) where
  ggen = fmap G.M1 <$> ggen


instance HasGen BaseField
instance HasGen SpecificField
instance HasGen RangeField
instance HasGen Int where
  gen = Gen.int Range.linearBounded
instance HasGen Text where
  gen = genAlpha
instance HasGen CronField
instance HasGen a => HasGen (NE.NonEmpty a) where
  gen = Gen.nonEmpty (Range.linear 1 50) gen
instance HasGen a => HasGen [a] where
  gen = Gen.list (Range.linear 0 50) gen
instance HasGen StepField where
  gen = Gen.just (mkStepField <$> gen <*> gen)
instance HasGen CronSchedule
instance HasGen Crontab
instance HasGen CronCommand where
  gen = CronCommand <$> genAlpha
instance HasGen CrontabEntry
instance HasGen MinuteSpec where
  gen = Gen.just (mkMinuteSpec <$> gen)
instance HasGen HourSpec where
  gen = Gen.just (mkHourSpec <$> gen)
instance HasGen DayOfMonthSpec where
  gen = Gen.just (mkDayOfMonthSpec <$> gen)
instance HasGen DayOfWeekSpec where
  gen = Gen.just (mkDayOfWeekSpec <$> gen)
instance HasGen MonthSpec where
  gen = Gen.just (mkMonthSpec <$> gen)
instance HasGen UTCTime where
  gen = genUTCTime

genAlpha :: Gen Text
genAlpha = Gen.text (Range.linear 1 50) Gen.alpha


genUTCTime :: Gen UTCTime
genUTCTime = genUTCTime' (Range.linear 0 maxBound)


-- | genUTCTime with a range of posix seconds
genUTCTime' :: Range.Range Int -> Gen UTCTime
genUTCTime' = fmap POSIX.posixSecondsToUTCTime . genPOSIXTime


genPOSIXTime :: Range.Range Int -> Gen POSIX.POSIXTime
genPOSIXTime rnge = fromInteger . toInteger <$> Gen.int rnge


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
