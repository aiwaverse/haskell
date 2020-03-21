module Main where

import           Conversions
import           System.Environment

main :: IO ()
main = do
  [base,number] <- getArgs
  let result = baseConvert (read base :: Integer) number
  putStrLn $ number ++ " in base " ++ base ++ " is equal to " ++ result
