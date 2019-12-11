module Game where

import Model (GameState)
import qualified GameState as GS
import qualified CommandParser as Parser
import qualified Commands
import Control.Monad.RWS.Lazy (RWS)
import qualified Control.Monad.RWS.Lazy as RWS

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
        let (newState, messageToPlayer) = run command state
        putStrLn $ unlines messageToPlayer
        game $ newState
    Nothing         -> game state

run :: Commands.Command -> GameState -> (GameState, [String])
run command state = RWS.execRWS (Commands.doCommand command) GS.gameMap state
