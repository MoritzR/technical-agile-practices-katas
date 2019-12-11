module Model where

import Data.Map (Map)
import Control.Monad.RWS.Lazy (RWS)
import qualified Control.Monad.RWS.Lazy as RWS

type Katacombs r = RWS GameMap [String] GameState r
tellPlayer :: String -> Katacombs ()
tellPlayer s = RWS.tell [s]
getMap :: Katacombs GameMap
getMap = RWS.ask
getState :: Katacombs GameState
getState = RWS.get
setState :: GameState -> Katacombs ()
setState = RWS.put

data GameState = GameState {
    playerAt :: Coordinate,
    items :: ItemLocations
}

type GameMap = Map Coordinate Location

data Command = Go Direction
    | Look Direction
    | LookAt ItemName
    | Take ItemName
    | Bag
    deriving (Show, Eq)

data Location = Location {
    title :: String,
    description :: String
} deriving (Show, Eq)


data Direction = North | South | West | East
    deriving (Show, Eq)

type Coordinate = (Int, Int)

data Item = Item {
    itemName :: ItemName,
    itemDescription :: String
}
instance Show Item where
    show item = show $ itemName item
instance Eq Item where
    itemA == itemB = itemName itemA == itemName itemB
instance Ord Item where
    compare itemA itemB = compare (itemName itemA) (itemName itemB)

newtype ItemName = ItemName String
    deriving (Eq, Ord)
instance Show ItemName where
    show (ItemName name) = name

type ItemLocations = Map Item ItemLocation

data ItemLocation = AtCoordinate Coordinate | InBag
    deriving (Show, Eq)