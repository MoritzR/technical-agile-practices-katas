module GameStateSpec (spec) where

import Test.Hspec
import GameState
import Model
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "commands" $ do
        let center = Location "center" ""
            east = Location "east" "the location to the east"
            west = Location "west" ""
            north = Location "north" "the location to the north"
            south = Location "south" ""
            gameMap = Map.fromList
                [ ((0, 0), center)
                , ((1, 0), east)
                , ((-1, 0), west)
                , ((0, 1), north)
                , ((0, -1), south)
                ]
            items = Map.fromList
                [ (Item (ItemName "a golden statue") "", (0, 1))
                , (Item (ItemName "a silver key") "", (0, 1))
                , (Item (ItemName "rusty key") "The head of this rusty key resembles a heart.", (0, 0))]
            state = GameState { playerAt = (0, 0), items = items }
            stateAfterCommand command = snd $ doCommand gameMap command state
            messageAfterCommand command = fst $ doCommand gameMap command state
        
        describe "go" $ do
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
            it "should also show a description of the items in the location" $ do
                let message = messageAfterCommand (Go North) 
                message `shouldBe` "north\nthe location to the north\nItems in the location: 'a golden statue' 'a silver key'"

        describe "look" $ do
            it "should display a message after looking north" $ do
                let message = messageAfterCommand (Look North)
                message `shouldBe` "You see the North"
            it "should display a message after looking south" $ do
                let message = messageAfterCommand (Look South)
                message `shouldBe` "You see the South"
        describe "lookAt" $ do
            it "should display a description after looking at an item" $ do
                let message = messageAfterCommand (LookAt $ ItemName "rusty key")
                message `shouldBe` "The head of this rusty key resembles a heart."