module Main where

import Lesson21

pizzaFunction :: IO ()
pizzaFunction = do
  putStrLn "What is the size of pizza 1"
  size1 <- getLine
  putStrLn "What is the cost of pizza 1"
  cost1 <- getLine
  putStrLn "What is the size of pizza 2"
  size2 <- getLine
  putStrLn "What is the cost of pizza 2"
  cost2 <- getLine
  let pizza1 = Pizza (read size1) (read cost1)
  let pizza2 = Pizza (read size2) (read cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn (describePizza betterPizza)

main :: IO ()
main = pizzaFunction

