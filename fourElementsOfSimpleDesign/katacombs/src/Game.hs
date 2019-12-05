module Game where

import GameState (GameState)
import qualified GameState as GS
import qualified CommandParser as Commands

startGame :: IO ()
startGame = game GS.initialGameState

game :: GameState -> IO ()
game gamestate = do
    putStr "> "
    input <- getLine
    putStrLn $ "You want to " ++ show (Commands.parse input)
    if input=="quit"
        then return ()
        else game gamestate
