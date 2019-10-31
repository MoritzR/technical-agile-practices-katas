import Test.Hspec

-- Game

newtype Board = Board String
  deriving (Eq, Show)
data Position = Middle | TopLeft | Bottom

emptyBoard = Board ""

place :: Position -> Board -> Board
place Middle _ = Board "--- -x- ---"
place TopLeft _ = Board "x-- --- ---"
place Bottom _ = Board "--- --- -x-"



    
-- Tests
main :: IO ()
main = hspec $ do
  describe "tic tac toe" $ do
    it "should place an x when placing first in the middle" $ do
      let board = emptyBoard

      let newBoard = place Middle emptyBoard
      
      newBoard `shouldBe` Board "--- -x- ---"
    it "should place an x when placing first on the top left" $ do
      let board = emptyBoard

      let newBoard = place TopLeft emptyBoard
      
      newBoard `shouldBe` Board "x-- --- ---"
    it "should place an x when placing first on the bottom" $ do
      let board = emptyBoard

      let newBoard = place Bottom emptyBoard
      
      newBoard `shouldBe` Board "--- --- -x-"