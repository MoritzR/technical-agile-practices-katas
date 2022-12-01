{-# HLINT ignore "Use map once" #-}
module Commands
    ( Command
    , doCommand
    , getPlayerLocation
    , displayItemsAtLocation ) where

import qualified Data.Map as Map
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Function ((&))
import qualified GameState as GS (bag)
import Control.Lens ((^.), (.=), (+=), (-=), at, _Just, Ixed (ix))
import Control.Monad (unless)
import Model
import Data.List (find)
import Control.Lens.Combinators (use)

doCommand :: Command -> Katacombs ()
doCommand (Go direction) = do
    moveIn direction
    doCommand LookAround

doCommand (Look toDirection) =
    tellPlayer $ "You see the " ++ show toDirection

doCommand (LookAt nameOfItem) = do
    let noItemMessage = "There is no '" ++ show nameOfItem ++ "' here."
    
    maybeItem <- findItemAtCurrentLocation nameOfItem
    maybeItem
        & maybe noItemMessage itemDescription 
        & tellPlayer

doCommand (Take nameOfItem) = do
    maybeFoundItem <- findItemAtCurrentLocation nameOfItem
    case maybeFoundItem of
        Just item   ->  do
            tellPlayer $ "You picked up " ++ show nameOfItem
            items.ix item .= InBag
        Nothing     ->  do
            tellPlayer $ "There is no '" ++ show nameOfItem ++ "' here."

doCommand (Drop nameOfItem) = do
    state <- getState
    let maybeFoundItem = find (\item -> itemName item == nameOfItem) (GS.bag state)
    case maybeFoundItem of
        Just item   ->  do
            tellPlayer $ "You dropped " ++ show nameOfItem
            items.ix item .= AtCoordinate (state^.playerAt)
        Nothing     ->  do
            tellPlayer $ "There is no '" ++ show nameOfItem ++ "' in your bag."

doCommand Bag = do
    state <- getState
    state
        & GS.bag
        & showItemsInBag
        & tellPlayer

doCommand LookAround = do
    location <- getPlayerLocation
    tellPlayer $ location^.title
    tellPlayer $ location^.description

    displayItemsAtLocation

findItemAtCurrentLocation :: ItemName -> Katacombs (Maybe Item)
findItemAtCurrentLocation nameOfItem = do
    itemsAtPlayerLocation <- getItemsAtPlayerLocation
    itemsAtPlayerLocation
        & find (\item -> itemName item == nameOfItem)
        & return

getPlayerLocation :: Katacombs Location
getPlayerLocation = do
    gameMap <- getMap
    playerPosition <- use playerAt
    case gameMap^.at playerPosition of
        Just location   -> return location
        Nothing         -> return $ Location "Limbo" "you shouldn't be here" -- TODO make this impossible

displayItemsAtLocation :: Katacombs ()
displayItemsAtLocation = do
    itemsAtPlayerLocation <- getItemsAtPlayerLocation
    let itemNames = itemsAtPlayerLocation
            & map itemName
            & map (\(ItemName name) -> "'" ++ name ++ "'")
    unless (null itemNames) $
        tellPlayer ("Items in the location: " ++ unwords itemNames)

moveIn :: Direction -> Katacombs ()
moveIn direction = case direction of
    North   -> playerAt.y += 1
    South   -> playerAt.y -= 1
    East    -> playerAt.x += 1
    West    -> playerAt.x -= 1

showItemsInBag :: [Item] -> String
showItemsInBag items
    | null items    = "The bag is empty."
    | otherwise     = "The bag contains: " ++ unwords names
        where names = items
                        & map itemName
                        & map (\(ItemName name) -> "'" ++ name ++ "'")

getItemsAtPlayerLocation :: Katacombs [Item]
getItemsAtPlayerLocation = do 
    state <- getState
    state^.items
        & Map.filter ((==) $ AtCoordinate $ state^.playerAt)
        & Map.keys
        & return