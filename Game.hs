import DataTypes
  (Player (..)
  , listToBoard
  )
import UI

main = print $ listToBoard [Circle | x <- [1..9]]
