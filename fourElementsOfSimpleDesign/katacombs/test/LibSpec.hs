module LibSpec (spec) where

import Test.Hspec
import Lib
import Data.Maybe (fromJust)

spec :: Spec
spec = do
  describe "parse go directions" $ do
    let parseJust = fromJust . parse
    it "should parse 'go n'" $
      parseJust "go n" `shouldBe` Go North
    it "should parse 'go s'" $
      parseJust "go s" `shouldBe` Go South
    it "should parse 'go w'" $
      parseJust "go w" `shouldBe` Go West
    it "should parse 'go e'" $
      parseJust "go e" `shouldBe` Go East
  it "should return Nothing for an unkown command" $
    parse "I want to be a butterfly" `shouldBe` Nothing