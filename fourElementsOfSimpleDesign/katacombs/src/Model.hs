module Model where

import Data.Map (Map)

data GameState = GameState {
    playerAt :: Coordinate,
    items :: ItemLocations
}

type GameMap = Map Coordinate Location

data Command = Go Direction
    | Look Direction
    | LookAt ItemName
    deriving (Show, Eq)

data Location = Location {
    title :: String,
    description :: String
} deriving (Show, Eq)


data Direction = North | South | West | East
    deriving (Show, Eq)

type Coordinate = (Int, Int)

newtype ItemName = ItemName String
    deriving (Show, Eq, Ord)

type ItemLocations = Map ItemName Coordinate