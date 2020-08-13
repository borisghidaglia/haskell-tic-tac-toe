import DataTypes
  (Player (..)
  , listToBoard
  , boardSize
  )
import UI
import Utils

initialBoard = listToBoard [Empty | x <- [1..9]]

main = play initialBoard

play :: Show a => Maybe a -> IO ()
play (Just b) = do
  putStrLn "Welcome in this tic-tac-toe game !"
  gameLoop
play Nothing = putStrLn "Board is invalid."

gameLoop :: IO ()
gameLoop = do
  n <- askInt
  case checkPosition n of
    Just n  -> putStrLn $ show n
    Nothing -> putStrLn $ "Position is invalid. Please enter a number between 1 and " ++ show boardSize

checkPosition :: Int -> Maybe Int
checkPosition n = if n <= boardSize && n > 0 then Just n else Nothing
