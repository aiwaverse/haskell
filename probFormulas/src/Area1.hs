module Area1
  ( combinations
  , fiveNumbers
  )
where

import           Data.List               hiding ( head
                                                , last
                                                )
import           Relude
combinations :: Int -> [a] -> [[a]]
combinations n xs = filter (\x -> n == length x) $ subsequences xs

fiveNumbers :: Ord a => [a] -> (Maybe a, Maybe a, Maybe a, Maybe a, Maybe a)
fiveNumbers xs = (ei, q1, md, q3, es)
 where
  sortedList = sort xs
  listSize   = genericLength sortedList
  ei         = viaNonEmpty head sortedList
  q1         = sortedList !!? (round . (subtract 1) $ (listSize + 1) / 4)
  md         = sortedList !!? (round . (subtract 1) $ 2 * (listSize + 1) / 4)
  q3         = sortedList !!? (round . (subtract 1) $ 3 * (listSize + 1) / 4)
  es         = viaNonEmpty last sortedList

