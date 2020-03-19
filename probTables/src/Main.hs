{-# LANGUAGE TypeApplications #-}
module Main where

import           Tables
import           Text.PrettyPrint.Tabulate     as T

main :: IO ()
main = do
  datas <- getLine
  let readyData = read @[Double] datas
  let dataList  = tableToList . buildTable . buildData $ readyData
  T.printTable dataList
