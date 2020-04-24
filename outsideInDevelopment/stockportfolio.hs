import Test.Hspec
import Data.Time.Calendar (Day, fromGregorian)
import Data.Function ((&))


type Company = String
type NumberOfShares = Int
type Shares = [Share]
data Share = Share Company NumberOfShares Day

newShares = []

buyShares :: Int -> String -> Day -> Shares -> Shares
buyShares amount company day existingShares = (Share company amount day):existingShares

displayShares :: Shares -> String
displayShares shares = "company | shares | current price | current value | last operation"


-- Tests
main :: IO ()
main = hspec $ do
  describe "Given a list of specific shares" $ do
    it "should display a specific portfolio" $ do
        let shares = newShares
                        & buyShares 1000 "Old School Waterfall Software LTD" (fromGregorian 1990 02 14)

        let result = displayShares shares
        
        result `shouldBe` "company | shares | current price | current value | last operation\n\
            \Old School Waterfall Software LTD | 500 | $5.75 | $2785.75 | sold 500 on 11/12/2018"