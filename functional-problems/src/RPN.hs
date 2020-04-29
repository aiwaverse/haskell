module RPN where

import           Data.List

solveRPN :: (Read a, Num a, Floating a) => String -> a
solveRPN = head . foldl' foldFunc [] . words
  where
    foldFunc (x : xs : xxs) "*"   = (xs * x) : xxs
    foldFunc (x : xs : xxs) "+"   = (xs + x) : xxs
    foldFunc (x : xs : xxs) "-"   = (xs - x) : xxs
    foldFunc (x : xs : xxs) "/"   = (xs / x) : xxs
    foldFunc (x : xs : xxs) "^"   = (xs ** x) : xxs
    foldFunc (x      : xs ) "ln"  = log x : xs
    foldFunc numberStack    "sum" = [sum numberStack]
    foldFunc numberStack    n     = read n : numberStack
