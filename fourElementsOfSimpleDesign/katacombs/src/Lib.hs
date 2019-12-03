module Lib where

game :: IO ()
game = putStrLn "Hello World"

parse :: String -> Command
parse "go n" = Go North
parse "go s" = Go South
parse "go w" = Go West
parse "go e" = Go East

data Command = Go Direction deriving (Show, Eq)
data Direction = North | South | West | East
    deriving (Show, Eq)