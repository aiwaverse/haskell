{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import Relude
import ReludeTest (someFunc)


main :: IO ()
main = putTextLn "Starting Program" >> someFunc 
 