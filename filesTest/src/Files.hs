module Files (Files.readFile) where

import qualified System.IO                     as S.IO


readFile :: String -> IO ()
readFile fileName = do
  handle   <- S.IO.openFile fileName S.IO.ReadMode
  contents <- S.IO.hGetContents handle
  mapM_ putStrLn $ lines contents
