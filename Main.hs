{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Attoparsec.Text
import System.Cron.Parser

main :: IO ()
main = mapM_ (parseTest cronSchedule) strs
  where strs = ["* * * * 13-7"]
