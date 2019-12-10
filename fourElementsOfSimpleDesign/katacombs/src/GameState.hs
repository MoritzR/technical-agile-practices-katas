module GameState where

import qualified Data.Map as Map
import Data.Function ((&))
import Model

bag :: GameState -> [Item]
bag state = items state
    & Map.filter ((==) InBag)
    & Map.keys

createItems :: [(Item, Coordinate)] -> ItemLocations
createItems list = list
    & map (\entry -> fmap AtCoordinate entry)
    & Map.fromList

initialGameState :: GameState
initialGameState = GameState {
    playerAt = (0, 0),
    items = createItems
        [   (Item {
                itemName = ItemName "rusted key",
                itemDescription = "A rusted key, the head of this key resembles a horse." }
            , (0, 0))]
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