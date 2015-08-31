module Main
    ( main
    ) where

import Control.Monad
import System.Cron
import System.Cron.Schedule

main :: IO ()
main = do
  void $ execSchedule $ do
    addJob (putStrLn "push")      "*/5 *   *   *   *"
  putStrLn "All cronjobs have been scheduled"
