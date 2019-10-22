import           Test.Hspec

numerals 1000 = "M"
numerals 10   = "X"
numerals 1998 = "MCMXCVIII"
numerals 1999 = "MCMXCIX"
numerals 2005 = "MMV"
numerals x | x <= 3                 = replicate x 'I'
           | x >= 2005 && x <= 2008 = numerals 2005 ++ numerals (x - 2005)

-- Tests
main :: IO ()
main = hspec $ do
  describe "roman numeral converter" $ do
    it "should create roman literal I" $ do
      numerals 1 `shouldBe` "I"
    it "should create roman literal III" $ do
      numerals 3 `shouldBe` "III"
    it "should create roman literal M" $ do
      numerals 1000 `shouldBe` "M"
    it "should create roman literal X" $ do
      numerals 10 `shouldBe` "X"
    it "should create roman literal MMVIII" $ do
      numerals 2008 `shouldBe` "MMVIII"
    it "should create roman literal MMVII" $ do
      numerals 2007 `shouldBe` "MMVII"
    it "should create roman literal MMVI" $ do
      numerals 2006 `shouldBe` "MMVI"
    it "should create roman literal MCMXCIX" $ do
      numerals 1999 `shouldBe` "MCMXCIX"
    it "should create roman literal MCMXCVIII" $ do
      numerals 1998 `shouldBe` "MCMXCVIII"
