module Main where
-- TODO: this *should* just work with {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec.Monadic

import qualified System.CronSpec
import qualified System.Cron.ParserSpec

main :: IO ()
main = hspecX $ do
  describe "System.Cron"        System.CronSpec.spec
  describe "System.Cron.Parser" System.Cron.ParserSpec.spec
