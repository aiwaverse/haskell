module Lesson22 where

import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Maybe                     ( fromJust )


solveEq :: String -> String -> String -> Int
solveEq n1 "+" n2 = read n1 + read n2
solveEq n1 "*" n2 = read n1 * read n2


simpleCalc :: String -> String
simpleCalc input = show $ solveEq n1 op n2 where [n1, op, n2] = words input



quotes :: [String]
quotes =
  [ "Hey! No"
  , "Alright, alright"
  , "Bad boy, bad boy"
  , "Alright 1 2 5 uh"
  , "yojeum na jogeum paenik sanghwang (Ha)"
  ]

giveQuote :: [String] -> [String]
giveQuote []         = []
giveQuote ("n" : xs) = []
giveQuote (x   : xs) = getQuote x : giveQuote xs
  where getQuote x = quotes !! (read x - 1)


quoteGiver :: IO ()
quoteGiver = do
  input <- getContents
  traverse_ putStrLn (giveQuote . lines $ input)


