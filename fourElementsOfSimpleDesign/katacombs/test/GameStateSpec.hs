module GameStateSpec (spec) where

import Test.Hspec
import GameState
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "commands" $ do
        describe "go" $ do
            let center = Location "center" ""
                east = Location "east" ""
                west = Location "west" ""
                north = Location "north" ""
                south = Location "south" ""
                gameMap = Map.fromList
                    [ ((0, 0), center)
                    , ((1, 0), east)
                    , ((-1, 0), west)
                    , ((0, 1), north)
                    , ((0, -1), south)
                    ]
                state = GameState { playerAt = (0, 0) }
    
            it "should move to the north" $ do
                let nextState = doCommand (Go North) state 
                getPlayerLocation gameMap nextState `shouldBe` north
            it "should move to the south" $ do
                let nextState = doCommand (Go South) state 
                getPlayerLocation gameMap nextState `shouldBe` south
            it "should move to the east" $ do
                let nextState = doCommand (Go East) state 
                getPlayerLocation gameMap nextState `shouldBe` east
            it "should move to the west" $ do
                let nextState = doCommand (Go West) state 
                getPlayerLocation gameMap nextState `shouldBe` west