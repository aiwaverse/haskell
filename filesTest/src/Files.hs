{-# LANGUAGE ScopedTypeVariables #-}
module Files (fileOps, view, write, remove) where

import           Data.Foldable                  ( traverse_ )
import           Data.List
import           System.Directory
import           System.Environment
import           System.IO

fileOps :: [(String, [String] -> IO ())]
fileOps = [("view", view), ("write", write), ("remove", remove)]

view :: [String] -> IO ()
view [fileName] = do
  fileHandle   <- openFile fileName ReadMode
  fileContents <- hGetContents fileHandle
  let fileLines     = lines fileContents
      numberedLines = zipWith (\l n -> show n ++ " - " ++ l) fileLines [1 ..]
  mapM_ putStrLn numberedLines

write :: [String] -> IO ()
write (fileName : contents) = mapM_ (appendFile fileName . (++ "\n")) contents


remove :: [String] -> IO ()
remove [fileName, taskNumber] = do
  fileHandle             <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  orgContents            <- hGetContents fileHandle
  let fileLines = lines orgContents
      newLines =
        [ l
        | (l, n) :: (String, Int) <- zip fileLines [1 ..]
        , n /= read taskNumber
        ]
  traverse_ (hPutStrLn tempHandle) newLines
  hClose fileHandle
  hClose tempHandle  
  removeFile fileName
  renameFile tempName fileName
