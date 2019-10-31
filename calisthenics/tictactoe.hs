import Test.Hspec

-- Game

newtype Board = Board String
  deriving (Eq, Show)
data Position =   TopLeft | Top | TopRight |
                  Middle | Bottom
  deriving Show

emptyBoard = Board ""

place :: Position -> Board -> Board
place Middle _ = Board "--- -x- ---"
place TopLeft _ = Board "x-- --- ---"
place TopRight _ = Board "--x --- ---"
place Bottom _ = Board "--- --- -x-"
place Top _ = Board "-x- --- ---"



    
-- Tests
main :: IO ()
main = hspec $ do
  describe "tic tac toe" $ do
    describe "placing on the first turn" $ do
      let test (position, board) = it ("should place an x in the " ++ show position) $ place position emptyBoard `shouldBe` board
          testAll = foldl1 (>>) . map test
          examples = [
            (TopLeft, Board "x-- --- ---"),
            (Top, Board "-x- --- ---"),
            (TopRight, Board "--x --- ---"),
            (Middle, Board "--- -x- ---"),
            (Bottom, Board "--- --- -x-")]
  
      testAll examples