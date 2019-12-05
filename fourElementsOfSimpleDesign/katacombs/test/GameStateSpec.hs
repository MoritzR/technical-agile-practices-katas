module GameStateSpec (spec) where

import Test.Hspec
import GameState
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "commands" $ do
        describe "go" $ do
            let center = Location "center" ""
                east = Location "east" "the location to the east"
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
                stateAfterCommand command = snd $ doCommand gameMap command state
                messageAfterCommand command = fst $ doCommand gameMap command state
    
            it "should move to the north" $ do
                let nextState = stateAfterCommand (Go North) 
                getPlayerLocation gameMap nextState `shouldBe` north
            it "should move to the south" $ do
                let nextState = stateAfterCommand (Go South) 
                getPlayerLocation gameMap nextState `shouldBe` south
            it "should move to the east" $ do
                let nextState = stateAfterCommand (Go East) 
                getPlayerLocation gameMap nextState `shouldBe` east
            it "should move to the west" $ do
                let nextState = stateAfterCommand (Go West) 
                getPlayerLocation gameMap nextState `shouldBe` west
            
            it "should also return title and description of the new location" $ do
                let message = messageAfterCommand (Go East) 
                message `shouldBe` "east\nthe location to the east"