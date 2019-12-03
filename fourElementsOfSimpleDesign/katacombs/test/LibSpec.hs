module LibSpec (spec) where

import Test.Hspec
import Lib

spec :: Spec
spec =
  describe "parse go directions" $ do
    it "should parse 'go n'" $
        Lib.parse "go n" `shouldBe` Go North
    it "should parse 'go s'" $
        Lib.parse "go s" `shouldBe` Go South
    it "should parse 'go w'" $
        Lib.parse "go w" `shouldBe` Go West
    it "should parse 'go e'" $
        Lib.parse "go e" `shouldBe` Go East