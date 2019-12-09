module CommandParser (parse) where

import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP (ReadP, choice, readP_to_S, string)
import qualified Text.ParserCombinators.ReadP as Parser
import Model

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
    return $ LookAt (ItemName item)

direction :: ReadP Direction
direction = do
    d <- Parser.get
    case d of
        'n' -> return North
        's' -> return South
        'w' -> return West
        'e' -> return East
        _   -> Parser.pfail
