module Lib where

import Data.List (isPrefixOf)
import Text.ParserCombinators.ReadP (ReadP, choice, readP_to_S, string)
import qualified Text.ParserCombinators.ReadP as Parser
import Data.Maybe (listToMaybe)
import GameState (GameState)
import qualified GameState as GS

startGame :: IO ()
startGame = game GS.initialGameState

game :: GameState -> IO ()
game gamestate = do
    let location = GS.currentLocation gamestate
    putStrLn $ GS.title location
    putStrLn $ GS.description location
    putStr "> "
    input <- getLine
    putStrLn $ "You want to " ++ show (parse input)
    if input=="quit"
        then return ()
        else game gamestate

parse :: String -> Maybe Command
parse = listToMaybe . map fst . readP_to_S command

command :: ReadP Command
command = do
    c <- choice [goCommand, lookCommand, lookAtCommand]
    Parser.eof
    return c

goCommand :: ReadP Command
goCommand = do
    string "go "
    d <- direction
    return $ Go d

lookCommand :: ReadP Command
lookCommand = do
    string "look "
    d <- direction
    return $ Look d

lookAtCommand :: ReadP Command
lookAtCommand = do
    string "look "
    item <- Parser.many1 Parser.get
    return $ LookAt (Itemname item)

direction :: ReadP Direction
direction = do
    d <- Parser.get
    case d of
        'n' -> return North
        's' -> return South
        'w' -> return West
        'e' -> return East
        _   -> Parser.pfail

data Command = Go Direction
    | Look Direction
    | LookAt Itemname
    deriving (Show, Eq)

data Direction = North | South | West | East
    deriving (Show, Eq)

newtype Itemname = Itemname String
    deriving (Show, Eq)