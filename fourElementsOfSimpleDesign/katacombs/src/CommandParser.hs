module CommandParser (parse) where

import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP (ReadP, choice, readP_to_S, string)
import qualified Text.ParserCombinators.ReadP as Parser
import Prelude hiding (take, drop)
import Model

parse :: String -> Maybe Command
parse = listToMaybe . map fst . readP_to_S command

command :: ReadP Command
command = do
    c <- choice 
        [ go
        , look
        , lookAt
        , lookAround
        , take
        , drop
        , bag
        ]
    Parser.eof
    return c

go :: ReadP Command
go = do
    string "go "
    d <- direction
    return $ Go d

look :: ReadP Command
look = do
    string "look "
    d <- direction
    return $ Look d

lookAt :: ReadP Command
lookAt = do
    string "look "
    item <- Parser.many1 Parser.get
    return $ LookAt (ItemName item)

lookAround :: ReadP Command
lookAround = do
    string "look"
    return LookAround

take :: ReadP Command
take = do
    string "take "
    item <- Parser.many1 Parser.get
    return $ Take (ItemName item)

drop :: ReadP Command
drop = do
    string "drop "
    item <- Parser.many1 Parser.get
    return $ Drop (ItemName item)

bag :: ReadP Command
bag = do
    string "bag"
    return Bag

direction :: ReadP Direction
direction = do
    d <- Parser.get
    case d of
        'n' -> return North
        's' -> return South
        'w' -> return West
        'e' -> return East
        _   -> Parser.pfail
