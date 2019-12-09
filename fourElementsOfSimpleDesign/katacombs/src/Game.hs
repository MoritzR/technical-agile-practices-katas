module Game where

import Model (GameState)
import qualified GameState as GS
import qualified CommandParser as Parser
import qualified Commands

startGame :: IO ()
startGame = do
    putStrLn "Katacombs of Shoreditch"
    game GS.initialGameState

game :: GameState -> IO ()
game state = do
    putStr "> "
    input <- getLine
    let commandFromPlayer = Parser.parse input
    putStrLn $ "You want to " ++ show commandFromPlayer
    if input=="quit"
        then return ()
        else continueWithUpdatedState commandFromPlayer state
    
continueWithUpdatedState playerCommand state = case playerCommand of
    Just command    -> do
        let (messageToPlayer, newState) = Commands.doCommand GS.gameMap command state
        putStrLn messageToPlayer
        game $ newState
    Nothing         -> game state 
