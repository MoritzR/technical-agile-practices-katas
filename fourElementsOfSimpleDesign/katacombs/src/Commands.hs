module Commands
    ( Command
    , doCommand
    , getPlayerLocation ) where

import qualified Data.Map as Map
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Function ((&))
import qualified GameState as GS (bag)
import Model

doCommand :: Command -> Katacombs ()
doCommand (Go direction) = do
    moveIn direction

    location <- getPlayerLocation
    tellPlayer $ title location
    tellPlayer $ description location

    displayItemsAtLocation

doCommand (Look toDirection) =
    tellPlayer $ "You see the " ++ show toDirection

doCommand (LookAt nameOfItem) = do
    gameMap <- getMap
    state <- getState
    tellPlayer $
        findItemAtCurrentLocation gameMap nameOfItem state
            & fmap itemDescription
            & fromMaybe ("There is no '" ++ show nameOfItem ++ "' here.")

doCommand (Take nameOfItem) = do
    gameMap <- getMap
    state <- getState
    let maybeFoundItem = findItemAtCurrentLocation gameMap nameOfItem state
    case maybeFoundItem of
        Just item   ->  do 
            tellPlayer $ "You picked up " ++ show nameOfItem
            setState $ state { items = Map.insert item InBag (items state)}
        Nothing     ->  do
            tellPlayer $ "There is no '" ++ show nameOfItem ++ "' here."

doCommand Bag = do
    state <- getState
    state
        & GS.bag
        & displayItemsInBag
        & tellPlayer

findItemAtCurrentLocation :: GameMap -> ItemName -> GameState -> Maybe Item
findItemAtCurrentLocation gameMap nameOfItem state =
    items state
        & Map.filter ((==) $ AtCoordinate $ playerAt state)
        & Map.keys
        & filter (\item -> itemName item == nameOfItem)
        & listToMaybe

getPlayerLocation :: Katacombs Location
getPlayerLocation = do
    gameMap <- getMap
    state <- getState
    case Map.lookup (playerAt state) gameMap of
        Just location   -> return $ location
        Nothing         -> return $ Location "Limbo" "you shouldn't be here" -- TODO make this impossible

displayItemsAtLocation :: Katacombs ()
displayItemsAtLocation = do
    state <- getState
    let itemsAtLocation = items state
            & Map.filter ((==) $ AtCoordinate (playerAt state))
            & Map.keys
        itemNames = itemsAtLocation
            & map itemName
            & map (\(ItemName name) -> "'" ++ name ++ "'")
    if (not . null) itemNames
        then tellPlayer ("Items in the location: " ++ unwords itemNames)
        else return ()

moveIn :: Direction -> Katacombs ()
moveIn direction = do
    state <- getState
    let (x, y) = playerAt state
    let newCoordinate = case direction of
            North   -> (x, y+1)
            South   -> (x, y-1)
            East    -> (x+1, y)
            West    -> (x-1, y)
    setState $ state { playerAt = newCoordinate }

displayItemsInBag :: [Item] -> String
displayItemsInBag items
    | items == []   = "The bag is empty."
    | otherwise = "The bag contains: " ++ unwords names
        where names = map (\(ItemName name) -> "'" ++ name ++ "'") . map itemName $ items
