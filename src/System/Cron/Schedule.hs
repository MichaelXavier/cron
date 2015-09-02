{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE CPP                        #-}

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

    , MonadSchedule (..)

    , runSchedule
    , runScheduleT

    , execSchedule
    ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Concurrent
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Attoparsec.Text   (parseOnly)
import           Data.Text              (pack)
import           Data.Time
import           System.Cron
import           System.Cron.Parser
#if !MIN_VERSION_time(1,5,0)
import           System.Locale
#endif

readTime' :: TimeLocale -> String -> String -> UTCTime
#if MIN_VERSION_time(1,5,0)
readTime' =  parseTimeOrError True
#else
readTime' = readTime
#endif

{- Scheduleing monad -}

data Job t = Job (CronSchedule t) (IO ())
type Jobs t = [Job t]

instance Show (Job t) where
    show (Job c _) = "(Job " ++ show c ++ ")"

data ScheduleError = ParseError String
                   deriving (Show)

type Schedule = ScheduleT Identity

newtype ScheduleT m t a = ScheduleT { unSchedule :: StateT (Jobs t) (ExceptT ScheduleError m) a }
        deriving ( Functor, Applicative, Monad
                 , MonadState (Jobs t)
                 , MonadError ScheduleError
                 )

runSchedule :: Schedule t a -> Either ScheduleError (a, [Job t])
runSchedule = runIdentity . runScheduleT

runScheduleT :: ScheduleT m t a -> m (Either ScheduleError (a, [Job t]))
runScheduleT = runExceptT . flip runStateT [] . unSchedule

class MonadSchedule m where
    addJob ::  IO () -> String -> m ()

instance (Monad m) => MonadSchedule (ScheduleT m t) where
    addJob a t = do s :: Jobs t <- get
                    case parseOnly cronSchedule (pack t) of
                        Left  e  -> throwError $ ParseError e
                        Right t' -> put $ Job t' a : s

{- Monitoring Engine -}

-- | Schedule all of the jobs to run at appropriate intervals. Each 
-- job that is launched gets a scheduling thread to itself. Each 
-- time a scheduling thread launches a job, the job is forked onto
-- a new thread. This means that if a job throws an excpetion in IO,
-- its thread will be killed, but it will continue to be scheduled 
-- in the future.
execSchedule :: CronTime t => Schedule t () -> IO [ThreadId]
execSchedule s = let res = runSchedule s
                  in case res of
                        Left  e         -> print e >> return []
                        Right (_, jobs) -> mapM forkJob jobs

-- | Start a job-runner thread that runs a job at appropriate 
-- intervals. Each time it is run, a new thread is forked for it,
-- meaning that a single exception does not take down the
-- scheduler.
forkJob :: forall t. CronTime t => Job t -> IO ThreadId
forkJob (Job s a) = forkIO $ forever $ do
            (timeAt, delay) <- findNextMinuteDelay
            timeAt' <- fromUTC timeAt :: IO t
            threadDelay delay
            when (scheduleMatches s timeAt') (void $ forkIO a)

findNextMinuteDelay :: IO (UTCTime, Int)
findNextMinuteDelay = do
        now <- getCurrentTime
        let f     = formatTime defaultTimeLocale fmtFront now
            m     = (read (formatTime defaultTimeLocale fmtMinutes now) :: Int) + 1
            r     = f ++ ":" ++ if length (show m) == 1 then "0" ++ show m else show m
            next  = readTime' defaultTimeLocale fmtRead r :: UTCTime
            diff  = diffUTCTime next now
            delay = round (realToFrac (diff * 1000000) :: Double) :: Int
        return (next, delay)
    where fmtFront   = "%F %H"
          fmtMinutes = "%M"
          fmtRead    = "%F %H:%M"
