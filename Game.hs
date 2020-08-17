import DataTypes
  (Player (..)
  , nextPlayer
  , Board (..)
  , listToBoard
  , boardSide
  , boardSize
  , Position
  )
import UI

initialBoard :: Maybe Board
initialBoard = listToBoard [Empty | _ <- [1..boardSize]]

main :: IO ()
main = play initialBoard

play :: Maybe Board -> IO ()
play (Just b) = do
  putStrLn "\n\nWelcome in this tic-tac-toe game !"
  gameLoop b Cross
play Nothing = putStrLn "Board is invalid."

gameLoop :: Board -> Player -> IO ()
gameLoop b p = do
  putStrLn $ "\nIt's " ++ show p ++ " turn.\n" ++ show b
  n <- askInt
  case checkPosition n of
    Just n' -> case setMark b p n' of
      Just b' -> case checkBoard b' p of
        Just sb -> gameLoop sb p' where
          p' = nextPlayer p
        Nothing -> do
          print b'
          putStrLn $ "Player " ++ show p ++ " won !"
      Nothing -> do
        putStrLn $ show n' ++ " is already taken! Please pick something else."
        gameLoop b p
    Nothing -> do
      putStrLn $ "Please enter a number between 1 and " ++ show boardSize
      gameLoop b p

checkBoard :: Board -> Player -> Maybe Board
checkBoard b@(Board ps) p =
  if checkCols || checkLines || checkDiags
  then Nothing
  else Just (Board ps)
  where
    aT          = elem True
    cp          = checkPattern b p
    leftD       = [x + y | (x,y) <- zip [0, boardSide..boardSize-1] [0..boardSide-1]]
    rightD      = [x - y | (x,y) <- zip [boardSide, boardSide*2..boardSize] [1..boardSide]]
    checkCols   = aT [cp [x, x+boardSide..boardSize-1] | x <- [0..boardSide-1]]
    checkLines  = aT [cp [x..x+boardSide-1] | x <- [0, boardSide..boardSize-1]]
    checkDiags  = aT [cp x | x <- [leftD,rightD]]

checkPattern :: Board -> Player -> [Int] -> Bool
checkPattern (Board ps) p xs = length (filter (\x -> ps !! x == p) xs) == boardSide

checkPosition :: Int -> Maybe Position
checkPosition n = if n <= boardSize && n > 0 then Just n else Nothing

setMark :: Board -> Player -> Position -> Maybe Board
setMark (Board ps) p n =
  if ps !! (n - 1) == Empty
  then listToBoard [if i == n then p else v | (v, i) <- zip ps [1..boardSize]]
  else Nothing
