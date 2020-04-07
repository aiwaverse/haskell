#!/usr/bin/env stack
-- this program has a major bug that i just don't really care to fix, it will convert ALL
-- instances of " . " to ".", instead of, ideally, only changing the ones that were like that
-- in the original file
-- aka don't use this, not yet
module Main where

import qualified Data.Text          as T
import qualified Data.Text.IO       as T.IO
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
