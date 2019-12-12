module CommandsSpec (spec) where

import Test.Hspec
import Commands
import Model
import Data.Function ((&))
import qualified Data.Map as Map
import qualified GameState as GS
import qualified Control.Monad.RWS.Lazy as RWS

spec :: Spec
spec = do
    let center = Location "center" ""
        east = Location "east" "the location to the east"
        west = Location "west" ""
        north = Location "north" "the location to the north"
        south = Location "south" ""
        gameMap = GS.createGameMap
            [ ((0, 0), center)
            , ((1, 0), east)
            , ((-1, 0), west)
            , ((0, 1), north)
            , ((0, -1), south)
            ]
        items = GS.createItems
            [ (Item (ItemName "a golden statue") "", (0, 1))
            , (Item (ItemName "a silver key") "", (0, 1))
            , (Item (ItemName "rusty key") "The head of this rusty key resembles a heart.", (0, 0))
            , (Item (ItemName "flute") "The flute is a musical instrument.", (0, 0))]
        state = GameState { _playerAt = Coordinate 0 0, _items = items }
        run action = RWS.execRWS action gameMap state
        stateAfterCommand command = fst $ run (doCommand command)
        messageAfterCommand command = snd $ run (doCommand command)
        newPlayerLocation newState = fst $ RWS.evalRWS getPlayerLocation gameMap newState
    
    describe "go" $ do
        it "should move to the north" $ do
            let nextState = stateAfterCommand (Go North) 
            newPlayerLocation nextState `shouldBe` north
        it "should move to the south" $ do
            let nextState = stateAfterCommand (Go South) 
            newPlayerLocation nextState `shouldBe` south
        it "should move to the east" $ do
            let nextState = stateAfterCommand (Go East) 
            newPlayerLocation nextState `shouldBe` east
        it "should move to the west" $ do
            let nextState = stateAfterCommand (Go West) 
            newPlayerLocation nextState `shouldBe` west
        
        it "should also return title and description of the new location" $ do
            let message = messageAfterCommand (Go East) 
            message `shouldBe` ["east", "the location to the east"]
        it "should also show a description of the items in the location" $ do
            let message = messageAfterCommand (Go North) 
            message `shouldBe` [ "north", "the location to the north", "Items in the location: 'a golden statue' 'a silver key'"]

    describe "look" $ do
        it "should display a message after looking north" $ do
            let message = messageAfterCommand (Look North)
            message `shouldBe` ["You see the North"]
        it "should display a message after looking south" $ do
            let message = messageAfterCommand (Look South)
            message `shouldBe` ["You see the South"]
    describe "lookAt" $ do
        it "should display a description after looking at an item" $ do
            let message = messageAfterCommand (LookAt $ ItemName "rusty key")
            message `shouldBe` ["The head of this rusty key resembles a heart."]
        it "should display a description when there is no such item to look at" $ do
            let message = messageAfterCommand (LookAt $ ItemName "item that is not there")
            message `shouldBe` ["There is no 'item that is not there' here."]
    describe "take" $ do
        it "should display a message when picking up an item" $ do
            let message = messageAfterCommand (Take $ ItemName "rusty key")
            message `shouldBe` ["You picked up rusty key"]
        it "should display a message when there is no such item in the location" $ do
            let message = messageAfterCommand (Take $ ItemName "item that is not there")
            message `shouldBe` ["There is no 'item that is not there' here."]
        it "should add the item to the player inventory" $ do
            let nextState = stateAfterCommand (Take $ ItemName "rusty key")
                itemsInBag = map itemName $ GS.bag nextState
            itemsInBag `shouldBe` [ItemName "rusty key"]
    describe "drop" $ do
        it "should display a message when dropping an item" $ do
            let message = snd $ run $ do
                    doCommand (Take $ ItemName "rusty key")
                    doCommand (Drop $ ItemName "rusty key")
            message `shouldContain` ["You dropped rusty key"]
    describe "bag" $ do
        it "should display the names of the items in the bag sorted alphabetically" $ do
            let message = snd $ run $ do
                    doCommand (Take $ ItemName "rusty key")
                    doCommand (Take $ ItemName "flute")
                    doCommand Bag
            message `shouldContain` ["The bag contains: 'flute' 'rusty key'"]
        it "should display a message when the bag is empty" $ do
            let message = messageAfterCommand (Bag)
            message `shouldBe` ["The bag is empty."]