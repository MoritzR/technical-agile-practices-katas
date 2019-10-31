import Test.Hspec

-- Game

newtype Board = Board String
  deriving (Eq, Show)

emptyBoard = Board ""

place "middle" _ = Board "--- -x- ---"
place "top-left" _ = Board "x-- --- ---"



    
-- Tests
main :: IO ()
main = hspec $ do
  describe "tic tac toe" $ do
    it "should place an x when placing first in the middle" $ do
      let board = emptyBoard

      let newBoard = place "middle" emptyBoard
      
      newBoard `shouldBe` Board "--- -x- ---"
    it "should place an x when placing first on the top left" $ do
      let board = emptyBoard

      let newBoard = place "top-left" emptyBoard
      
      newBoard `shouldBe` Board "x-- --- ---"