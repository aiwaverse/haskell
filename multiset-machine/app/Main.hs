module Main (main) where

import MultisetMachine (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
