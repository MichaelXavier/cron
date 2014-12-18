{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

--------------------------------------------------------------------
-- |
-- Module      : System.Cron.Schedule
-- Description : Monad stack for scheduling jobs to be executed by cron rules.
-- Copyright   : (c) Andrew Rademacher 2014
-- License     : MIT
--
-- Maintainer: Andrew Rademacher <andrewrademacher@gmail.com>
-- Portability: portable
--
-- > main :: IO ()
-- > main = do
-- >        ...
-- >        tids <- execSchedule $ do
-- >            addJob job1 "* * * * *"
-- >            addJob job2 "0 * * * *"
-- >        print tids
-- >        ...
-- >
-- > job1 :: IO ()
-- > job1 = putStrLn "Job 1"
-- >
-- > job2 :: IO ()
-- > job2 = putStrLn "Job 2"
--
--------------------------------------------------------------------

module System.Cron.Schedule
    ( Job (..)
    , ScheduleError (..)
    , Schedule
    , ScheduleT (..)

    , runSchedule
    , runScheduleT

    , execSchedule
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Attoparsec.Text   (parseOnly)
import           Data.Text              (pack)
import           Data.Time
import           System.Cron
import           System.Cron.Parser
import           System.Locale

{- Scheduleing monad -}

data Job = Job CronSchedule (IO ())
type Jobs = [Job]

instance Show Job where
    show (Job c _) = "(Job " ++ show c ++ ")"

data ScheduleError = ParseError String
                   deriving (Show)

type Schedule = ScheduleT Identity

newtype ScheduleT m a = ScheduleT { unSchedule :: StateT Jobs (ExceptT ScheduleError m) a }
        deriving ( Functor, Applicative, Monad
                 , MonadState Jobs
                 , MonadError ScheduleError
                 )

runSchedule :: Schedule a -> Either ScheduleError (a, [Job])
runSchedule = runIdentity . runScheduleT

runScheduleT :: ScheduleT m a -> m (Either ScheduleError (a, [Job]))
runScheduleT = runExceptT . flip runStateT [] . unSchedule

class MonadSchedule m where
    addJob ::  IO () -> String -> m ()

instance (Monad m) => MonadSchedule (ScheduleT m) where
    addJob a t = do s :: Jobs <- get
                    case parseOnly cronSchedule (pack t) of
                        Left  e  -> throwError $ ParseError e
                        Right t' -> put $ Job t' a : s

{- Monitoring Engine -}

execSchedule :: Schedule () -> IO [ThreadId]
execSchedule s = let res = runSchedule s
                  in case res of
                        Left  e         -> print e >> return []
                        Right (_, jobs) -> mapM forkJob jobs

forkJob :: Job -> IO ThreadId
forkJob (Job s a) = forkIO $ do
        findNextMinuteDelay >>= threadDelay
        forever $ do
            now <- getCurrentTime
            when (scheduleMatches s now) a
            threadDelay $ 1000000 * 60

findNextMinuteDelay :: IO Int
findNextMinuteDelay = do
        now <- getCurrentTime
        let f    = formatTime defaultTimeLocale fmtFront now
            m    = (read (formatTime defaultTimeLocale fmtMinutes now) :: Int) + 1
            r    = f ++ ":" ++ if length (show m) == 1 then "0" ++ show m else show m
            next = readTime defaultTimeLocale fmtRead r :: UTCTime
        newNow <- getCurrentTime    -- Compensates for the time spent calculating what the next minute is.
        let diff  = diffUTCTime next newNow
            delay = round (realToFrac (diff * 1000000) :: Double) :: Int
        return delay
    where fmtFront   = "%F %H"
          fmtMinutes = "%M"
          fmtRead    = "%F %H:%M"
