module GameState where

import qualified Data.Map as Map (fromList)
import Model

initialGameState :: GameState
initialGameState = GameState {
    playerAt = (0, 0),
    items = Map.fromList
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