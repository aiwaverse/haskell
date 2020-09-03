module Area1(combinations) where

import Data.List

combinations :: Int -> [a] -> [[a]]
combinations n xs = filter (\x -> n == length x) $ subsequences xs
