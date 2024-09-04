module System.Cron.Internal.Describe.Time where

import System.Cron.Internal.Describe.Types
import Data.Time (formatTime, TimeOfDay (TimeOfDay), defaultTimeLocale, TimeLocale, TimeZone, utc, utcToLocalTimeOfDay)

newtype Minute = Minute Int
newtype Hour   = Hour Int

format :: TimeFormat -> Minute -> Hour -> String
format Hour24 = fmtTime utc defaultTimeLocale "%R"
format Hour12 = fmtTime utc defaultTimeLocale "%I:%M %p"
format (CustomTimeFormat zone locale fmt) = fmtTime zone locale fmt

fmtTime :: TimeZone -> TimeLocale -> String -> Minute -> Hour -> String
fmtTime zone locale fmt (Minute m) (Hour h) = formatTime locale fmt tod
  where
    tod = snd . utcToLocalTimeOfDay zone $ TimeOfDay h m 0