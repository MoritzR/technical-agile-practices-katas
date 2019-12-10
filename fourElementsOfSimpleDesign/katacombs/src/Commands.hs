module Commands
    ( doCommand
    , getPlayerLocation ) where

import qualified Data.Map as Map
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Function ((&))
import qualified GameState as GS (bag)
import Model

doCommand :: GameMap -> Command -> GameState -> (MessageToPlayer, GameState)
doCommand gameMap (Go toDirection) state =
    ( title newLocation ++ "\n" ++ description newLocation
        ++ displayItemsAtLocation (Map.keys (Map.filter ((==) $ AtCoordinate newCoordinate) (items state)))
    , newState
    )
        where   newCoordinate   = moveTo toDirection (playerAt state)
                newState        = state { playerAt = newCoordinate }
                newLocation     = getPlayerLocation gameMap newState
doCommand gameMap (Look toDirection) state =
    ("You see the " ++ show toDirection, state)
doCommand gameMap (LookAt nameOfItem) state =
    ( findItemAtCurrentLocation gameMap nameOfItem state
        & fmap itemDescription
        & fromMaybe ("There is no '" ++ show nameOfItem ++ "' here.")
    , state)
doCommand gameMap (Take nameOfItem) state =
    case maybeFoundItem of
        Just item   ->  ( "You picked up " ++ show nameOfItem
                        , state { items = Map.insert item InBag (items state)})
        Nothing     ->  ("There is no '" ++ show nameOfItem ++ "' here."
                        , state)
    where maybeFoundItem = findItemAtCurrentLocation gameMap nameOfItem state
doCommand _gameMap Bag state =
    ( displayItemsInBag $ GS.bag state
    , state)

findItemAtCurrentLocation :: GameMap -> ItemName -> GameState -> Maybe Item
findItemAtCurrentLocation gameMap nameOfItem state =
    items state
        & Map.filter ((==) $ AtCoordinate $ playerAt state)
        & Map.keys
        & filter (\item -> itemName item == nameOfItem)
        & listToMaybe

getPlayerLocation :: GameMap -> GameState -> Location
getPlayerLocation map state = case Map.lookup (playerAt state) map of
    Just location   -> location
    Nothing         -> Location "Limbo" "you shouldn't be here" -- TODO make this impossible

moveTo :: Direction -> Coordinate -> Coordinate
moveTo direction (x, y) = case direction of
    North   -> (x, y+1)
    South   -> (x, y-1)
    East    -> (x+1, y)
    West    -> (x-1, y)

type MessageToPlayer = String

displayItemsAtLocation :: [Item] -> String
displayItemsAtLocation items
    | items == []       = ""
    | otherwise         = "\nItems in the location: " ++ unwords names
        where names = map (\(ItemName name) -> "'" ++ name ++ "'") . map itemName $ items

displayItemsInBag :: [Item] -> String
displayItemsInBag items
    | items == []   = "The bag is empty."
    | otherwise = "The bag contains: " ++ unwords names
        where names = map (\(ItemName name) -> "'" ++ name ++ "'") . map itemName $ items
