module Main (main) where

import MultisetMachine

main :: IO ()
main = do
  input <- getLine
  let register = map (\c -> read @Int $ [c]) input
  putStrLn $ printBitMultiset . program . fromList $ register

ins1 :: BitMultiset -> BitMultiset
ins1 s = ins2 $ deleteBit Zero s

ins2 :: BitMultiset -> BitMultiset
ins2 s = if onlyOnes s then ins3 s else ins1 s

ins3 :: BitMultiset -> BitMultiset
ins3 s = ins4 $ deleteBit Zero . deleteBit One $ s

ins4 :: BitMultiset -> BitMultiset
ins4 s = if onlyOnes s then ins3 s else s

program :: BitMultiset -> BitMultiset
program s
  | nZero > nOne = insereBit Zero $ ins1 . insereBit Zero $ s
  | nZero < nOne = insereBit One $ ins1 s
  | otherwise = ins1 s
  where
    nZero = length . filter (== Zero) $ s
    nOne = length . filter (== One) $ s