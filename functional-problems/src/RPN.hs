module RPN where

import           Data.List

solveRPN :: (Read a, Num a) => String -> a
solveRPN = head . foldl' foldFunc [] . words
  where
    foldFunc (x : xs : xxs) "*" = (x * xs) : xxs
    foldFunc (x : xs : xxs) "+" = (x + xs) : xxs
    foldFunc (x : xs : xxs) "-" = (xs - x) : xxs
    foldFunc numberStack    n   = read n : numberStack
