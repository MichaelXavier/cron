module Main
    ( main
    ) where



-------------------------------------------------------------------------------
import           SpecHelper
import qualified System.Test.Cron
import qualified System.Test.Cron.Parser
import qualified System.Test.Cron.Schedule
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain $ testGroup "cron"
  [ System.Test.Cron.tests
  , System.Test.Cron.Parser.tests
  , System.Test.Cron.Schedule.tests
  ]
