module System.Cron.ScheduleSpec
    ( spec
    ) where

import           Control.Concurrent
import           Test.Hspec

import           System.Cron.Schedule

spec :: Spec
spec = sequence_ [ describeMonadSchedule
                 , describeExecSchedule
                 ]

empty :: IO ()
empty = return ()

flipMVar :: MVar String -> IO ()
flipMVar v = putMVar v "dost thou even hoist"

describeMonadSchedule :: Spec
describeMonadSchedule = describe "MonadSchedule" $ do
    let Right ((), s) = runSchedule (do addJob empty "* * * * *"
                                        addJob empty "0 * * * *"
                                        addJob empty "0 0 * * *")
    it "should place the first job as the last job stated." $
        let (Job x _) = (s !! 0)
         in show x `shouldBe` "CronSchedule 0 0 * * *"
    it "should place the last job as the first job stated." $
        let (Job x _) = (s !! 2)
         in show x `shouldBe` "CronSchedule * * * * *"
    it "should read all three jobs." $
        length s `shouldBe` 3

describeExecSchedule :: Spec
describeExecSchedule = describe "execSchedule" $ do
    it "should set an mvar each minute" $
        fireAndWait >>= (`shouldBe` "dost thou even hoist")
    where fireAndWait = do
            v    <- newEmptyMVar
            tids <- execSchedule $ do
                addJob (flipMVar v) "* * * * *"
            threadDelay (1000000 * 60)
            mapM_ killThread tids
            takeMVar v
