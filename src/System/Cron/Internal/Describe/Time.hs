module System.Cron.Internal.Describe.Time where

import System.Cron.Internal.Describe.Types

newtype Minute = Minute Int
newtype Hour   = Hour Int

format :: TimeFormat -> Minute -> Hour -> String
format t (Minute m) (Hour h) = leftPad h ++ ":" ++ leftPad m ++ suffix t
  where leftPad n
          | n < 10    = "0" ++ show n
          | otherwise = show n
        suffix Hour24 = ""
        suffix Hour12
          | h < 12    = " AM"
          | otherwise = " PM"
