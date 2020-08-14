module DataTypes
  (Player (..)
  , nextPlayer
  , Board (..)
  , listToBoard
  , boardSide
  , boardSize
  , Position
  ) where

data Player = Cross | Circle | Empty deriving (Eq)

instance Show Player where
  show Cross = "X"
  show Circle = "O"
  show Empty = "_"

makeLine :: [Player] -> String
makeLine ps = x ++ "\n\n" where
  x = unwords [" " ++ show p ++ " " | p <- ps]

nextPlayer :: Player -> Player
nextPlayer p = if p == Cross then Circle else Cross



newtype Board = Board [Player]

boardSide :: Int
boardSide = 3
boardSize :: Int
boardSize = boardSide * boardSide

instance Show Board where
  show = showBoard

listToBoard :: [Player] -> Maybe Board
listToBoard xs = if length xs == boardSize then Just (Board xs) else Nothing

showBoard :: Board -> String
showBoard (Board []) = []
showBoard (Board ps) = x ++ y where
  x = makeLine $ take 3 ps
  y = showBoard (Board (drop 3 ps))



type Position = Int
