import Test.Hspec    

fib 0 = 0
fib 1 = 1
fib 2 = 1


-- Tests
main :: IO ()
main = hspec $ do
  describe "fib" $ do
    it "returns 0 at index 0" $ do
        fib 0 `shouldBe` 0
    it "returns 1 at index 1" $ do
        fib 1 `shouldBe` 1
    it "returns 1 at index 2" $ do
        fib 2 `shouldBe` 1
    it "returns 2 at index 2" $ do
        fib 3 `shouldBe` 2