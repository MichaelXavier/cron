{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Monad (void, forever)
import           Control.Monad.State (put)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)
import           System.Process (CreateProcess(..), createProcess, waitForProcess, proc)

import           System.Cron
import           System.Cron.Parser
import           System.Cron.Schedule

die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure

jobs :: Crontab -> [Job]
jobs (Crontab entries) = map ($ shellJob environ) commandFuns
  where
    (environ, commandFuns) = foldr apart ([], []) entries

    apart entry (env', cmds) =
      case entry of
       CommandEntry sched (T.unpack -> com) -> (env', Job sched . ($ com):cmds)
       EnvVariable (T.unpack -> k) (T.unpack -> v) -> ((k, v):env', cmds)

shellJob :: [(String, String)] -> String -> IO ()
shellJob environ shell = void $ do
  (_, _, _, handle) <- createProcess create
  waitForProcess handle
  where
    create = (proc "/bin/sh" ["-c", shell]) { env = anyEnv environ }
    anyEnv [] = Nothing
    anyEnv xs = Just xs

readCrontab :: FilePath -> IO Crontab
readCrontab file = do
  text <- T.readFile file
  let Right (tab, leftovers) = A.parseOnly ((,) <$> crontab <*> A.takeText) text
  case leftovers of
   "" -> return tab
   _ -> die $ "Could not parse crontab, stopped at: " ++ take 100 (T.unpack leftovers)

usage :: IO a
usage = do
  name <- getProgName
  die $ "usage: " ++ name ++ " [-t] /path/to/crontab"

go :: Crontab -> IO ()
go tab = do
  void $ execSchedule $ put $ jobs tab
  forever $ threadDelay maxBound

main :: IO ()
main = do
  args <- getArgs
  case args of
   ["-t", file] -> void $ readCrontab file
   [file] -> readCrontab file >>= go
   _ -> usage
