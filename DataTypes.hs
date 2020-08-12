module DataTypes
  (Player (..)
  , listToBoard
  ) where

data Player = Cross | Circle

instance Show Player where
  show Cross = "X"
  show Circle = "O"


boardSize = 3^2
newtype Board = Board [Player]

instance Show Board where
  show = showBoard

listToBoard :: [Player] -> Maybe Board
listToBoard xs = if length xs == boardSize then Just (Board xs) else Nothing

showBoard :: Board -> String
showBoard _ = "This is a Board"
