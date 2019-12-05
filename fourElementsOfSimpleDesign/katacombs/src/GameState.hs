module GameState 
    ( GameState (..)
    , Location (..)
    , GameMap (..)
    , Command (..)
    , Direction (..)
    , Itemname (..)
    , Coordinate
    , initialGameState
    , doCommand
    , getPlayerLocation) where

import Data.Map (Map)
import qualified Data.Map as Map

doCommand :: Command -> GameState -> GameState
doCommand (Go toDirection) state = state

getPlayerLocation :: GameMap -> GameState -> Location
getPlayerLocation map state = Location "north" ""

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

gameMap = Map.fromList
    [ ((0, 0), Location {
            title = "Jail Cell",
            description = "You are standing in a jail cell. A faint light reaches you from a small shaft in the ceiling."
        })
    ]