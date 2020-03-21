module Main where

import           Data.Maybe
import           Files
import           System.Environment

main :: IO ()
main = do
  (fileName : operation : opArgs) <- getArgs
  let op = fromJust $ lookup operation fileOps
  op (fileName : opArgs)
