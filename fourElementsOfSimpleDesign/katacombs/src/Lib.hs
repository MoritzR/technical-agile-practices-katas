module Lib where

game :: IO ()
game = putStrLn "Hello World"

parse :: String -> Command
parse _ = Go North

data Command = Go Direction deriving (Show, Eq)
data Direction = North deriving (Show, Eq)