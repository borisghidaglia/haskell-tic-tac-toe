module UI where

import Text.Read

askInt :: IO Int
askInt = do
        putStrLn "Please enter the postion number:"
        s <- getLine
        case readMaybe s of
          Just n ->  return n
          Nothing -> putStrLn "That's not a number! Try again" >> askInt
