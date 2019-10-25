import Test.Hspec    

factors 2 = [2]


-- Tests
main :: IO ()
main = hspec $ do
  describe "splitting a number into prime factors" $ do
    it "should split two" $ do
        factors 2 `shouldBe` [2]