import Test.Hspec    

factors 4 = [2,2]
factors x = [x]


-- Tests
main :: IO ()
main = hspec $ do
  describe "splitting a number into prime factors" $ do
    let test (number, primeFactors) = it ("should split " ++ show number ++ " into " ++ show primeFactors) $ factors number `shouldBe` primeFactors
        testAll = foldl1 (>>) . map test
        examples = [
            (2, [2]),
            (3, [3]),
            (4, [2,2])
            ]

    testAll examples 