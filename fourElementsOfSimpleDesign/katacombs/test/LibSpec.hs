module LibSpec (spec) where

import Test.Hspec
import Lib

spec :: Spec
spec =
  describe "parse go directions" $ do
    it "should parse 'go n'" $
        Lib.parse "go n" `shouldBe` Go North