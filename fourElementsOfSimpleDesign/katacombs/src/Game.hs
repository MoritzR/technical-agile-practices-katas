module Game where

import GameState (GameState)
import qualified GameState as GS
import qualified CommandParser as Commands

startGame :: IO ()
startGame = do
    putStrLn "Katacombs of Shoreditch"
    game GS.initialGameState

game :: GameState -> IO ()
game gameState = do
    putStr "> "
    input <- getLine
    let commandFromPlayer = Commands.parse input
    putStrLn $ "You want to " ++ show commandFromPlayer
    if input=="quit"
        then return ()
        else case commandFromPlayer of
            Just command    -> game $ GS.doCommand command gameState
            Nothing         -> game gameState
