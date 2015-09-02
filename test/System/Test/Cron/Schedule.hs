{-# LANGUAGE ScopedTypeVariables #-}
module System.Test.Cron.Schedule
    ( tests
    ) where

-------------------------------------------------------------------------------
import           Control.Concurrent
import           Data.Proxy (Proxy(..))
-------------------------------------------------------------------------------
import           SpecHelper
import           System.Cron.Schedule
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "System.Cron.Schedule"
  [ describeMonadSchedule
  , describeExecSchedule
  ]

flipMVar :: MVar String -> IO ()
flipMVar v = putMVar v "dost thou even hoist"

describeMonadSchedule :: TestTree
describeMonadSchedule = testGroup "MonadSchedule"
  [
    testCase "should place the first job as the last job stated." $
      let (Job x _) = (s !! 0)
       in show x @?= "CronSchedule 0 0 * * *"
  , testCase "should place the last job as the first job stated." $
      let (Job x _) = (s !! 2)
       in show x @?= "CronSchedule * * * * *"
  , testCase "should read all three jobs." $
      length s @?= 3
  ]
  where Right ((), s) = runSchedule $ do addJob empty "* * * * *"
                                         addJob empty "0 * * * *"
                                         addJob empty "0 0 * * *"

describeExecSchedule :: TestTree
describeExecSchedule = testGroup "execSchedule"
  [
     testCase "should set an mvar each minute (UTCTime)" $
       fireAndWait (Proxy :: Proxy UTCTime) >>= (@?= "dost thou even hoist")
   , testCase "should set an mvar each minute (LocalTime)" $
       fireAndWait (Proxy :: Proxy LocalTime) >>= (@?= "dost thou even hoist")
  ]
    where
      fireAndWait :: forall t. CronTime t => Proxy t -> IO String
      fireAndWait _ = do
            v    <- newEmptyMVar
            tids <- execSchedule $ do
                (addJob (flipMVar v) "* * * * *" :: Schedule t ())
            threadDelay (1000000 * 60)
            mapM_ killThread tids
            takeMVar v
