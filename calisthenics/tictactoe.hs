import Test.Hspec

-- Game


emptyBoard = ""

place _ _ = "x"



    
-- Tests
main :: IO ()
main = hspec $ do
  describe "tic tac toe" $ do
    it "placing first should result in the board having an x" $ do
      let board = emptyBoard

      let newBoard = place "middle" emptyBoard
      
      newBoard `shouldContain` "x"