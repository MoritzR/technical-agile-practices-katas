module GameState 
    ( GameState (..)
    , Location (..)
    , GameMap (..)
    , Command (..)
    , Direction (..)
    , Itemname (..)
    , Coordinate
    , initialGameState
    , gameMap
    , doCommand
    , getPlayerLocation) where

import Data.Map (Map)
import qualified Data.Map as Map

doCommand :: GameMap -> Command -> GameState -> (MessageToPlayer, GameState)
doCommand gameMap (Go toDirection) state =
    ( title newLocation ++ "\n" ++ description newLocation
    , GameState { playerAt = newCoordinate }
    )
        where   newCoordinate   = moveTo toDirection (playerAt state)
                newState        = GameState { playerAt = newCoordinate }
                newLocation     = getPlayerLocation gameMap newState
doCommand gameMap (Look toDirection) state =
    ("You see the North", state)

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

data GameState = GameState {
    playerAt :: Coordinate
}

type GameMap = Map Coordinate Location

type Coordinate = (Int, Int)

data Location = Location {
    title :: String,
    description :: String
} deriving (Eq)

instance Show Location where
    show l = "Location '" ++ title l ++ "'"

data Command = Go Direction
    | Look Direction
    | LookAt Itemname
    deriving (Show, Eq)

data Direction = North | South | West | East
    deriving (Show, Eq)

newtype Itemname = Itemname String
    deriving (Show, Eq)


initialGameState = GameState {
    playerAt = (0, 0)
}

gameMap :: GameMap
gameMap = Map.fromList
    [ ((0, 0), Location {
            title = "Jail Cell",
            description = "You are standing in a jail cell. A faint light reaches you from a small shaft in the ceiling."
        })
    ]