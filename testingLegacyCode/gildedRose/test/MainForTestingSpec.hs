module MainForTestingSpec (spec) where

import Test.Hspec
import MainForTesting

spec :: Spec
spec =
  describe "golden master" $ do
    it "reports main test coverage" $ do
      main