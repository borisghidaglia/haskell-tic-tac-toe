module DataTypes
  (Player (..)
  , listToBoard
  ) where

data Player = Cross | Circle

instance Show Player where
  show Cross = "X"
  show Circle = "O"

makeLine :: [Player] -> String
makeLine ps = x ++ "\n\n" where
  x = unwords [" " ++ show p ++ " " | p <- ps]


boardSize = 3^2
newtype Board = Board [Player]

instance Show Board where
  show = showBoard

listToBoard :: [Player] -> Maybe Board
listToBoard xs = if length xs == boardSize then Just (Board xs) else Nothing

showBoard :: Board -> String
showBoard (Board []) = []
showBoard (Board ps) = x ++ y where
  x = makeLine $ take 3 ps
  y = showBoard (Board (drop 3 ps))
