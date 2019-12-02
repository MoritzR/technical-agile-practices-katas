module LibSpec (spec) where

import Test.Hspec

spec :: Spec
spec =
  describe "equality" $ do
    it "should equal 3 and 3" $
        3 `shouldBe` 3