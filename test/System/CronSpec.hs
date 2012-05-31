module System.CronSpec (spec) where
-- TODO: this *should* just work with {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec.Monadic

spec :: Spec
spec = sequence_ [ describeTruth ]

---- Specs
describeTruth :: Spec
describeTruth = describe "truth" $ do
  it "is obvious" $ True == False
