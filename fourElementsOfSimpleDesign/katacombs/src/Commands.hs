module Commands
    ( Command
    , doCommand
    , getPlayerLocation ) where

import qualified Data.Map as Map
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Function ((&))
import qualified GameState as GS (bag)
import Control.Lens ((^.), (.=), (+=), (-=))
import Model

doCommand :: Command -> Katacombs ()
doCommand (Go direction) = do
    moveIn direction

    location <- getPlayerLocation
    tellPlayer $ location^.title
    tellPlayer $ location^.description

    displayItemsAtLocation

doCommand (Look toDirection) =
    tellPlayer $ "You see the " ++ show toDirection

doCommand (LookAt nameOfItem) = do
    maybeItem <- findItemAtCurrentLocation nameOfItem
    maybeItem
        & fmap itemDescription
        & fromMaybe ("There is no '" ++ show nameOfItem ++ "' here.")
        & tellPlayer

doCommand (Take nameOfItem) = do
    maybeFoundItem <- findItemAtCurrentLocation nameOfItem
    state <- getState
    case maybeFoundItem of
        Just item   ->  do 
            tellPlayer $ "You picked up " ++ show nameOfItem
            items .= (Map.insert item InBag (state^.items))
        Nothing     ->  do
            tellPlayer $ "There is no '" ++ show nameOfItem ++ "' here."

doCommand Bag = do
    state <- getState
    state
        & GS.bag
        & displayItemsInBag
        & tellPlayer

findItemAtCurrentLocation :: ItemName -> Katacombs (Maybe Item)
findItemAtCurrentLocation nameOfItem = do
    state <- getState
    state^.items
        & Map.filter ((==) $ AtCoordinate $ state^.playerAt)
        & Map.keys
        & filter (\item -> itemName item == nameOfItem)
        & listToMaybe
        & return

getPlayerLocation :: Katacombs Location
getPlayerLocation = do
    gameMap <- getMap
    state <- getState
    case Map.lookup (state^.playerAt) gameMap of
        Just location   -> return $ location
        Nothing         -> return $ Location "Limbo" "you shouldn't be here" -- TODO make this impossible

displayItemsAtLocation :: Katacombs ()
displayItemsAtLocation = do
    state <- getState
    let itemsAtLocation = state^.items
            & Map.filter ((==) $ AtCoordinate $ state^.playerAt)
            & Map.keys
        itemNames = itemsAtLocation
            & map itemName
            & map (\(ItemName name) -> "'" ++ name ++ "'")
    if (not . null) itemNames
        then tellPlayer ("Items in the location: " ++ unwords itemNames)
        else return ()

moveIn :: Direction -> Katacombs ()
moveIn direction = case direction of
    North   -> playerAt.y += 1
    South   -> playerAt.y -= 1
    East    -> playerAt.x += 1
    West    -> playerAt.x -= 1

displayItemsInBag :: [Item] -> String
displayItemsInBag items
    | items == []   = "The bag is empty."
    | otherwise = "The bag contains: " ++ unwords names
        where names = map (\(ItemName name) -> "'" ++ name ++ "'") . map itemName $ items
