module Main (main) where

import StackTest (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
