{- |
Copyright: (c) 2021 aiwa
SPDX-License-Identifier: MIT
Maintainer: aiwa <aiwavision@protonmail.com>

See README for more info
-}

{-#LANGUAGE ViewPatterns#-}

module MultisetMachine
    ( printBitMultiset
    , Bit
    , BitMultiset
    , printBit
    , zeroGTOne
    , oneGTZero
    , onlyOnes
    , onlyZeros
    , insereBit
    , deleteBit
    ) where

import Data.List (foldl', delete)

projectName :: String
projectName = "multiset-machine"

data Bit = One | Zero deriving (Eq, Ord)

type BitMultiset = [Bit]

printBitMultiset :: BitMultiset -> String
printBitMultiset = concatMap printBit

printBit :: Bit -> String
printBit One = "1"
printBit Zero = "0"

zeros :: BitMultiset -> Int
zeros = length . filter (== Zero)

ones :: BitMultiset -> Int
ones = length . filter (== One)

zeroGTOne :: BitMultiset -> Bool
zeroGTOne s = zeros s > ones s 

oneGTZero :: BitMultiset -> Bool
oneGTZero s = ones s > zeros s

onlyZeros :: BitMultiset -> Bool
onlyZeros s = o == (0 :: Int) && z >= (1 :: Int)
    where
        (z, o) = foldl' (\(x, y) l -> if l == Zero then (x + 1, y) else (x, y + 1)) (0, 0) s

onlyOnes :: BitMultiset -> Bool
onlyOnes s = z == (0 :: Int) && o >= (1 :: Int)
    where
        (z, o) = foldl' (\(x, y) l -> if l == Zero then (x + 1, y) else (x, y + 1)) (0, 0) s

invert :: Bit -> Bit
invert One = Zero
invert Zero = One

insereBit :: Bit -> BitMultiset -> BitMultiset
insereBit b = invertOrNot . (b :)
    where
        otherBitCount = if b == Zero then ones else zeros
        invertOrNot xs = if length xs > otherBitCount xs then map invert xs else xs 

deleteBit :: Bit -> BitMultiset -> BitMultiset
deleteBit b = invertOrNot . delete b
    where
        otherBitCount = if b == Zero then ones else zeros
        invertOrNot xs = if length xs > otherBitCount xs then map invert xs else xs 
