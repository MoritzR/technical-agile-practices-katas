module GameState 
    ( GameState (..)
    , Location (..)
    , Command (..)
    , Direction (..)
    , Itemname (..)
    , initialGameState) where

data GameState = GameState {
    currentLocation :: Location
}

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

initalLocation = Location {
    title = "Jail Cell",
    description = "You are standing in a jail cell. A faint light reaches you from a small shaft in the ceiling."
}

initialGameState = GameState {
    currentLocation = initalLocation
}