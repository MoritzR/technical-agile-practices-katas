module GameState where

import qualified Data.Map as Map
import Data.Function ((&))
import Control.Lens ((^.))
import Model

bag :: GameState -> [Item]
bag state = state^.items
    & Map.filter ((==) InBag)
    & Map.keys

createItems :: [(Item, Coordinate)] -> ItemLocations
createItems list = list
    & map (\entry -> fmap AtCoordinate entry)
    & Map.fromList

initialGameState :: GameState
initialGameState = GameState {
    _playerAt = (0, 0),
    _items = createItems
        [   (Item {
                itemName = ItemName "rusted key",
                itemDescription = "A rusted key, the head of this key resembles a horse." }
            , (0, 0))]
}

gameMap :: GameMap
gameMap = Map.fromList
    [ ((0, 0), Location {
            _title = "Jail Cell",
            _description = "You are standing in a jail cell. A faint light reaches you from a small shaft in the ceiling."
        })
    , ((0, 1), Location {
        _title = "Corridor",
        _description = "A long corridor, with cells to each side."
        })
    ]