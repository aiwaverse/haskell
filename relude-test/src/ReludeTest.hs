{- |
Copyright: (c) 2020 Aiwa
SPDX-License-Identifier: MIT
Maintainer: Aiwa <aiwavision@protonmail.com>

See README for more info
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ReludeTest (someFunc, makeVector) where

import qualified Data.Vector.Unboxed           as VU
import           Relude                  hiding ( map )

someFunc :: IO ()
someFunc = putTextLn "someFunc"

-- |- Make an Int Unboxed vector out of a list of Ints
makeVector :: [Int] -> VU.Vector Int
makeVector = VU.unfoldr uncons

map' :: (a -> b) -> [a] -> [b]
map' _  []       = []
map' fn (x : xs) = fn x : map' fn xs


neMap :: (a -> b) -> Maybe (NonEmpty a) -> Maybe (NonEmpty b)
neMap fn (Just ne) = nonEmpty . map' fn . getList $ ne
neMap _  Nothing   = Nothing

getList :: NonEmpty a -> [a]
getList (x :| xs) = x : xs
