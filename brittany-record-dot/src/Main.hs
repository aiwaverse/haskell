#!/usr/bin/env stack
module Main where

import           Data.Foldable                  ( traverse_ )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import           System.Directory
import           System.Environment
import           System.IO
import           System.Process

main :: IO ()
main = do
  [fileName]             <- getArgs
  r                      <- readProcess "brittany" [fileName] []
  (tempName, tempHandle) <- openTempFile "." "britt"
  let parsedContents = T.replace (T.pack " . ") (T.pack ".") $ T.pack r
  T.IO.hPutStr tempHandle parsedContents
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
