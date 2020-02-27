{-# LANGUAGE CPP #-}

module System.Cron.Internal.Schedule where

import           Data.Time


findNextMinuteDelay' :: UTCTime -> (UTCTime, Int)
findNextMinuteDelay' now = (next, delay)
  where
    f     = formatTime defaultTimeLocale fmtFront now
    m     = (read (formatTime defaultTimeLocale fmtMinutes now) :: Int) + 1
    r     = f ++ ":" ++ if length (show m) == 1 then "0" ++ show m else show m
    next  = readTime' defaultTimeLocale fmtRead r :: UTCTime
    diff  = diffUTCTime next now
    delay = round (realToFrac (diff * 1000000) :: Double) :: Int
    fmtFront   = "%F %H"
    fmtMinutes = "%M"
    fmtRead    = "%F %H:%M"

readTime' :: TimeLocale -> String -> String -> UTCTime
#if MIN_VERSION_time(1,5,0)
readTime' =  parseTimeOrError True
#else
readTime' = readTime
#endif
