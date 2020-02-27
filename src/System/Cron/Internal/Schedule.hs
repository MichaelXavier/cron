{-# LANGUAGE CPP #-}

module System.Cron.Internal.Schedule (findNextMinuteDelay') where

import           Data.Time

findNextMinuteDelay' :: UTCTime -> (UTCTime, Int)
findNextMinuteDelay' now = (next, delay)
  where
    oneMinuteLater = addUTCTime oneMinute now
    plainMinute = truncateToPlainMinute $ utctDayTime oneMinuteLater
    next = oneMinuteLater { utctDayTime = plainMinute }
    diff = diffUTCTime next now
    delay = round (realToFrac (diff * 1000000) :: Double) :: Int

oneMinute :: NominalDiffTime
oneMinute = 60

truncateToPlainMinute :: DiffTime -> DiffTime
truncateToPlainMinute = fromIntegral . (* 60) . (`quot` 60) . truncate
