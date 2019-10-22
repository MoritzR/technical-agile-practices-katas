import Test.Hspec    

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


-- Tests
main :: IO ()
main = hspec $ do
  describe "fib returning elements of the fibonacci sequence" $ do
    it "returns the 0th element" $ do
        fib 0 `shouldBe` 0
    it "returns the 1st element" $ do
        fib 1 `shouldBe` 1
    it "returns the 2nd element" $ do
        fib 2 `shouldBe` 1
    it "returns the 3rd element" $ do
        fib 3 `shouldBe` 2
    it "returns the 9th element" $ do
        fib 9 `shouldBe` 34