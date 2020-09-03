module Area1
  ( combinations
  )
where

import           Data.List               hiding ( head )

combinations :: Int -> [a] -> [[a]]
combinations n xs = filter (\x -> n == length x) $ subsequences xs

fiveNumbers :: (Ord a, Num a) => [a] -> (Maybe a, Maybe a, Maybe a, Maybe a, Maybe a)
fiveNumbers xs = undefined
 where
  sortedList = sort xs
  listSize   = genericLength sortedList
  ei         = viaNonEmpty head xs
  q1         = sortedList !!? (round . (subtract 1) $ (listSize + 1) / 4)
  md         = sortedList !!? (round . (subtract 1) $ (listSize + 1) / 4)

