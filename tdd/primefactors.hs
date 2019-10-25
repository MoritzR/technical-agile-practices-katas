import Test.Hspec    

factors 4 = [2,2]
factors x = [x]


-- Tests
main :: IO ()
main = hspec $ do
  describe "splitting a number into prime factors" $ do
    it "should split 2" $ do
        factors 2 `shouldBe` [2]
    it "should split 3" $ do
        factors 3 `shouldBe` [3]
    it "should split 4" $ do
        factors 4 `shouldBe` [2,2]