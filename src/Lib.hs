module Lib
    ( someFunc
    ) where

readDhall :: FilePath -> IO ()
readDhall filePath = do
    putStrLn filePath

hello :: IO ()
hello = putStrLn "Hello World"

someFunc :: IO ()
someFunc = putStrLn "someFunc"
