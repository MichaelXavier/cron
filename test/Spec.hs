module Main where

import Test.Hspec.Monadic

import qualified System.CronSpec

main :: IO ()
main = hspecX $ do
  describe "Cron" System.CronSpec.spec
