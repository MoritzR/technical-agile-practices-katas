module LibSpec (spec) where

import Test.Hspec
import Lib
import Data.Maybe (fromJust)

spec :: Spec
spec = do
  describe "parsing commands" $ do
    let parseJust = fromJust . parse
    describe "parse go directions" $ do
      it "should parse 'go n'" $
        parseJust "go n" `shouldBe` Go North
      it "should parse 'go s'" $
        parseJust "go s" `shouldBe` Go South
      it "should parse 'go w'" $
        parseJust "go w" `shouldBe` Go West
      it "should parse 'go e'" $
        parseJust "go e" `shouldBe` Go East
    describe "parse look directions" $ do
      it "should parse 'look n'" $
        parseJust "look" `shouldBe` Look North
    it "should return Nothing for an unkown command" $
      parse "I want to be a butterfly" `shouldBe` Nothing