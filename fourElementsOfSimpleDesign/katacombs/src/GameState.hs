module GameState where

import qualified Data.Map as Map
import Data.Function ((&))
import Control.Lens ((^.))
import Model
import qualified Items
import qualified Data.Bifunctor as Bifunctor

bag :: GameState -> [Item]
bag state = state^.items
    & Map.filter (InBag ==)
    & Map.keys

createItems :: [(Item, (Int, Int))] -> ItemLocations
createItems list = list
    & map (fmap (AtCoordinate . uncurry Coordinate))
    & Map.fromList

createGameMap :: [((Int, Int), Location)] -> GameMap
createGameMap list = list
    & map (Bifunctor.first (uncurry Coordinate))
    & Map.fromList

initialGameState :: GameState
initialGameState = GameState {
    _playerAt = Coordinate 0 0,
    _items = createItems  [(Items.rustedKey, (0, 0))]
}

gameMap :: GameMap
gameMap = createGameMap
    [ ((0, 0), Location {
            _title = "Jail Cell",
            _description = "You are standing in a jail cell. A faint light reaches you from a small shaft in the ceiling."
        })
    , ((0, 1), Location {
        _title = "Corridor",
        _description = "A long corridor, with cells to each side."
        })
    , ((0, 2), Location {
        _title = "Corridor",
        _description = "A long corridor, with cells to each side and a closed door to the north."
        })
    ]