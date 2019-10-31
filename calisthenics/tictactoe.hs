import Test.Hspec

-- Game

newtype Board = Board String
  deriving (Eq, Show)

emptyBoard = Board ""

place _ _ = Board "--- -x- ---"



    
-- Tests
main :: IO ()
main = hspec $ do
  describe "tic tac toe" $ do
    it "placing first should result in the board having an x" $ do
      let board = emptyBoard

      let newBoard = place "middle" emptyBoard
      
      newBoard `shouldBe` Board "--- -x- ---"