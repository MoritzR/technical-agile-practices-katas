module GameState 
    ( initialGameState
    , gameMap
    , doCommand
    , getPlayerLocation) where

import Data.Map (Map)
import qualified Data.Map as Map
import Model

doCommand :: GameMap -> Command -> GameState -> (MessageToPlayer, GameState)
doCommand gameMap (Go toDirection) state =
    ( title newLocation ++ "\n" ++ description newLocation
        ++ displayItemsAtLocation (Map.keys (Map.filter ((==) newCoordinate) (items state)))
    , newState
    )
        where   newCoordinate   = moveTo toDirection (playerAt state)
                newState        = state { playerAt = newCoordinate }
                newLocation     = getPlayerLocation gameMap newState
doCommand gameMap (Look toDirection) state =
    ("You see the " ++ show toDirection, state)
doCommand gameMap (LookAt (ItemName name)) state =
    ("You see the " ++ name, state)

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

displayItemsAtLocation :: [ItemName] -> String
displayItemsAtLocation itemnames
    | names == []       = ""
    | otherwise         = "\nItems in the location: " ++ unwords names
        where names = map (\(ItemName name) -> "'" ++ name ++ "'") itemnames

initialGameState = GameState {
    playerAt = (0, 0),
    items = Map.fromList [(ItemName "rusted key", (0, 0))]
}

gameMap :: GameMap
gameMap = Map.fromList
    [ ((0, 0), Location {
            title = "Jail Cell",
            description = "You are standing in a jail cell. A faint light reaches you from a small shaft in the ceiling."
        })
    , ((0, 1), Location {
        title = "Corridor",
        description = "A long corridor, with cells to each side."
        })
    ]