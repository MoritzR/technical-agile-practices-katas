module LibSpec (spec) where

import Test.Hspec
import Lib
import Data.Maybe (fromJust)

spec :: Spec
spec = do
  describe "parsing commands" $ do
    let parseJust = fromJust . parse
        test (input, expectedCommand)
          = it ("should parse " ++ show input) $ parseJust input `shouldBe` expectedCommand
        testAll = foldl1 (>>) . map test
        examples = [
          ("go n", Go North),
          ("go s", Go South),
          ("go w", Go West),
          ("go e", Go East),

          ("look n", Look North)]

    testAll examples

    it "should return Nothing for an unkown command" $
      parse "I want to be a butterfly" `shouldBe` Nothing