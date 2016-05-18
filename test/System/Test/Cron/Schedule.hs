module System.Test.Cron.Schedule
    ( tests
    ) where

-------------------------------------------------------------------------------
import           Control.Concurrent
-------------------------------------------------------------------------------
import           SpecHelper
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
  where Right ((), s) = runSchedule $ do addJob noop "* * * * *"
                                         addJob noop "0 * * * *"
                                         addJob noop "0 0 * * *"
        noop = return ()

describeExecSchedule :: TestTree
describeExecSchedule = testGroup "execSchedule"
  [
     testCase "should set an mvar each minute" $
       fireAndWait >>= (@?= "dost thou even hoist")
  ]
    where fireAndWait = do
            v    <- newEmptyMVar
            tids <- execSchedule $ do
                addJob (flipMVar v) "* * * * *"
            threadDelay (1000000 * 60)
            mapM_ killThread tids
            takeMVar v
