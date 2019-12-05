module CommandParserSpec (spec) where

import Test.Hspec
import Lib
import Data.Maybe (fromJust)
import GameState (Command (..), Direction (..), Itemname (..))
import CommandParser

spec :: Spec
spec = do
  describe "parsing commands" $ do
    describe "successfully" $ do
      let parseJust = fromJust . parse
          test (input, expectedCommand)
            = it ("should parse " ++ show input) $
                parseJust input `shouldBe` expectedCommand
          testAll = foldl1 (>>) . map test
          examples = [
            ("go n", Go North),
            ("go s", Go South),
            ("go w", Go West),
            ("go e", Go East),

            ("look n", Look North),
            ("look s", Look South),
            ("look w", Look West),
            ("look e", Look East),

            ("look keys", LookAt (Itemname "keys")),
            ("look door", LookAt (Itemname "door"))]

      testAll examples

    describe "failing to parse returns 'Nothing'" $ do
      it "for input 'asjdoijoqwrn'" $
        parse "asjdoijoqwrn" `shouldBe` Nothing
      describe "for commands that have a valid command as prefix" $ do
        let test input
              = it (show input) $
                  parse input `shouldBe` Nothing
            testAll = foldl1 (>>) . map test
            examples = [
              "go not north",
              "go somewhere else",
              "go whereever",
              "go elsewhere"]
        testAll examples