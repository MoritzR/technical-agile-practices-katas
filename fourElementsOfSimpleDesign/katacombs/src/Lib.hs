module Lib where

game :: IO ()
game = putStrLn "Hello World"

parse :: String -> Maybe Command
parse "go n" = Just $ Go North
parse "go s" = Just $ Go South
parse "go w" = Just $ Go West
parse "go e" = Just $ Go East
parse _ = Nothing

data Command = Go Direction deriving (Show, Eq)
data Direction = North | South | West | East
    deriving (Show, Eq)