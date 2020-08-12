module Utils where

rawPrint :: Show a => Maybe a -> IO()
rawPrint (Just a) = print a
rawPrint a = print a
