import Test.Hspec    

factors n = [[2], [3], [2,2], [5], [2,3], [7], [2,2,2], [3,3], [], [], [2,2,3]] !! (n - 2)


-- Tests
main :: IO ()
main = hspec $ do
  describe "splitting a number into prime factors" $ do
    let test (number, primeFactors) = it ("should split " ++ show number ++ " into " ++ show primeFactors) $ factors number `shouldBe` primeFactors
        testAll = foldl1 (>>) . map test
        examples = [
            (2, [2]),
            (3, [3]),
            (4, [2,2]),
            (5, [5]),
            (6, [2,3]),
            (8, [2,2,2]),
            (9, [3,3]),
            (12, [2,2,3])
            ]

    testAll examples 