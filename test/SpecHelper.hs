{-# LANGUAGE TemplateHaskell #-}
module SpecHelper (module X) where

import Control.Applicative as X
import Data.Attoparsec.Text as X
import Data.DeriveTH
import Data.Time.Clock as X
import Data.Time.Calendar as X
import Data.Time.LocalTime as X
import Test.Hspec as X
import Test.Hspec.QuickCheck as X
import Test.QuickCheck as X hiding (Result)

import System.Cron as X
import System.Cron.Parser as X

import Debug.Trace as X

instance Arbitrary UTCTime where
  arbitrary = do
    d <- ModifiedJulianDay . fromInteger . getPositive <$> arbitrary
    t <- fromInteger . getPositive <$> arbitrary
    return $ UTCTime d t

$(derive makeArbitrary ''CronField)
$(derive makeArbitrary ''MinuteSpec)
$(derive makeArbitrary ''HourSpec)
$(derive makeArbitrary ''DayOfWeekSpec)
$(derive makeArbitrary ''DayOfMonthSpec)
$(derive makeArbitrary ''MonthSpec)
$(derive makeArbitrary ''CronSchedule)
