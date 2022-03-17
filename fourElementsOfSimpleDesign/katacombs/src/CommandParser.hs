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
    Go <$> direction

look :: ReadP Command
look = do
    string "look "
    Look <$> direction

lookAt :: ReadP Command
lookAt = do
    string "look "
    LookAt . ItemName <$> anyString

lookAround :: ReadP Command
lookAround = do
    string "look"
    return LookAround

take :: ReadP Command
take = do
    string "take "
    Take . ItemName <$> anyString

drop :: ReadP Command
drop = do
    string "drop "
    Drop . ItemName <$> anyString

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

anyString :: ReadP String
anyString = Parser.many1 Parser.get