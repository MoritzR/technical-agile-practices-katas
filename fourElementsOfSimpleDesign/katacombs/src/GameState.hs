module GameState (GameState, Location, initialGameState, title, description, currentLocation) where

data GameState = GameState {
    currentLocation :: Location
}

data Location = Location {
    title :: String,
    description :: String
}

initalLocation = Location {
    title = "Jail Cell",
    description = "You are standing in a jail cell. A faint light reaches you from a small shaft in the ceiling."
}

initialGameState = GameState {
    currentLocation = initalLocation
}