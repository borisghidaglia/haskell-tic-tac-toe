import DataTypes
  (Player (..)
  , listToBoard
  )
import Utils

initialBoard = listToBoard [Empty | x <- [1..9]]

main = play initialBoard

play :: Show a => Maybe a -> IO()
play Nothing = putStrLn "Board is invalid."
play (Just b) = do
  putStrLn "Welcome in this tic-tac-toe game !"
  rawPrint (Just b)
