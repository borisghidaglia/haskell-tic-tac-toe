import DataTypes
  (Player (..)
  , nextPlayer
  , Board (..)
  , listToBoard
  , boardSize
  , Position
  )
import UI
import Utils

initialBoard = listToBoard [Empty | x <- [1..9]]

main = play initialBoard

play :: Maybe Board -> IO ()
play (Just b) = do
  putStrLn "Welcome in this tic-tac-toe game !"
  gameLoop b Cross
play Nothing = putStrLn "Board is invalid."

gameLoop :: Board -> Player -> IO ()
gameLoop b p = do
  putStrLn $ "\n" ++ show b
  n <- askInt
  case checkPosition n of
    Just n  -> case setMark b p n of
      Just b' -> gameLoop b' p' where
        p' = nextPlayer p
      Nothing -> do
        putStrLn $ "Board is invalid."
        gameLoop b p
    Nothing -> do
      putStrLn $ "Position is invalid. Please enter a number between 1 and " ++ show boardSize
      gameLoop b p

checkPosition :: Int -> Maybe Position
checkPosition n = if n <= boardSize && n > 0 then Just n else Nothing

setMark :: Board -> Player -> Position -> Maybe Board
setMark (Board ps) p n = listToBoard [if i == n then p else v | (v, i) <- zip ps [1..boardSize]]
