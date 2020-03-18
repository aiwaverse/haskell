module Main where

import qualified Files                         as F

main :: IO ()
main = do
  putStrLn "Enter the file name"
  fileName <- getLine
  F.readFile fileName
