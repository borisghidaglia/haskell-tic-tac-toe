import DataTypes
  (Player (..)
  , listToBoard
  )
import Utils

main = rawPrint $ listToBoard [Circle | x <- [1..9]]
