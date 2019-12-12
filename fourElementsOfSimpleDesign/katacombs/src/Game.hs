module Game where

import Model (GameState, Command(LookAround))
import qualified GameState as GS
import qualified CommandParser as Parser
import qualified Commands
import Control.Monad.RWS.Lazy (RWS)
import qualified Control.Monad.RWS.Lazy as RWS

startGame :: IO ()
startGame = do
    putStrLn "Katacombs of Shoreditch"
    startGameWithLookingAround GS.initialGameState

startGameWithLookingAround :: GameState -> IO ()
startGameWithLookingAround state = do
    let (newState, messages) = run LookAround state
    putStrLn $ unlines messages
    game newState

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
