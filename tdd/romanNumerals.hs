import           Test.Hspec

numerals 1000 = "M"
numerals 5    = "V"
numerals 10   = "X"
numerals 1995 = "MCMXCV"
numerals 1999 = "MCMXCIX"
numerals 2005 = "MMV"
numerals x | x <= 3 = replicate x 'I'
           | lastDigit >= 5 && lastDigit <= 8 = partWithoutIs ++ partWithI
  where lastDigit = x `mod` 10
        partWithoutIs = numerals (x - lastDigit + 5)
        partWithI = numerals (lastDigit - 5)
        

-- Tests
main :: IO ()
main = hspec $ do
  describe "roman numeral converter" $ do
    let test (arabic, roman) = it ("should convert " ++ show arabic) $ numerals arabic `shouldBe` roman
        testAll = foldl1 (>>) . map test
        examples = [
          (1, "I"),
          (3, "III"),
          (1000, "M"),
          (10, "X"),
          (2008, "MMVIII"),
          (2007, "MMVII"),
          (2006, "MMVI"),
          (1999, "MCMXCIX"),
          (1998, "MCMXCVIII"),
          (1997, "MCMXCVII"),
          (1996, "MCMXCVI"),
          (8, "VIII"),
          (7, "VII"),
          (6, "VI")]

    testAll examples 
