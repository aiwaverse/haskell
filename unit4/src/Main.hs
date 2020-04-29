module Main where

import           Control.Monad
import           Lesson22
import           System.IO


main :: IO ()
main = quoteGiver

-- old main
{-
main :: IO ()
main = do
  input <- getLine
  unless (null input) $ do
    print $ simpleCalc input
    main
  when (null input) $ print "Goodbye"
-}
