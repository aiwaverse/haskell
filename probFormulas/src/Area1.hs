{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Area1
  ( combinations
  , fiveNumbers
  , discrepants
  , cleanDiscrepants
  , countOccurrences
  , median
  , firstQuartile
  , thirdQuartile
  , tanques
  )
where

import           Data.List               hiding ( head
                                                , last
                                                )
import           Relude
import           Relude.Unsafe                  ( fromJust )


tanques1 = [189, 220, 227, 231, 239, 257, 269, 290]
tanques2 = [195, 220, 229, 231, 249, 258, 269, 290]
tanques3 = [214, 222, 229, 232, 253, 259, 270, 313]
tanques4 = [218, 223, 230, 232, 253, 260, 274, 361]
tanques5 = [220, 224, 231, 237, 254, 268, 277, 375]
tanques :: [Double]
tanques = mconcat [tanques1, tanques2, tanques3, tanques4, tanques5]


-- | given the size of the combinations and a list, produces all the possible subsequences of that size
combinations :: Int -> [a] -> [[a]]
combinations n xs = filter (\x -> n == length x) $ subsequences xs

-- | counts the occurence of each value in a list, returns it ordered
countOccurrences :: Ord a => [a] -> [(a, Int)]
countOccurrences []  = []
countOccurrences lst = let (x : xs) = sort lst in internalCount x 1 xs
 where
  internalCount x n [] = [(x, n)]
  internalCount x n (xs : xss) =
    if (x /= xs) then (x, n) : internalCount xs 1 xss else internalCount x (n + 1) xss

-- | given a list, calculates the (inferior extreme, first quartile, median, third quartile, superior extreme)
fiveNumbers
  :: (Ord a, Fractional a) => [a] -> (Maybe a, Maybe a, Maybe a, Maybe a, Maybe a)
fiveNumbers xs = (ei, q1, md, q3, es)
 where
  sortedList = sort xs
  ei         = viaNonEmpty head sortedList
  q1         = firstQuartile sortedList
  md         = median sortedList
  q3         = thirdQuartile sortedList
  es         = viaNonEmpty last sortedList

firstQuartile :: (Ord a, Fractional a) => [a] -> Maybe a
firstQuartile (sort -> xs) = if odd (length xs)
  then mean (xs !!? ceiling pOdd, xs !!? floor pOdd)
  else mean (xs !!? ceiling pEven, xs !!? floor pEven)
  where
    pOdd = ((genericLength xs + 1) / 4) - 1
    pEven = ((genericLength xs + 2) / 4) - 1
    mean (Just x, Just y) = Just ((x + y) / 2)
    mean (Nothing, _) = Nothing
    mean (_, Nothing) = Nothing


thirdQuartile :: (Ord a, Fractional a) => [a] -> Maybe a
thirdQuartile (sort -> xs) = if odd (length xs)
  then mean (xs !!? ceiling pOdd, xs !!? floor pOdd)
  else mean (xs !!? ceiling pEven, xs !!? floor pEven)
  where
    pOdd = (3*(genericLength xs + 1) / 4) - 1
    pEven = ((3 * genericLength xs + 2) / 4) - 1
    mean (Just x, Just y) = Just ((x + y) / 2)
    mean (Nothing, _) = Nothing
    mean (_, Nothing) = Nothing


median :: (Ord a, Fractional a) => [a] -> Maybe a
median (sort -> xs) = if odd listSize
  then sort xs !!? (listSize `div` 2)
  else liftA2 (\x y -> (x + y) / 2)
              (sort xs !!? (listSize `div` 2))
              (sort xs !!? ((listSize `div` 2) - 1))
  where listSize = length xs

-- | calculates the superior and inferior discrepant values given
-- the five numbers
discrepants
  :: (Ord a, Fractional a)
  => (Maybe a, Maybe a, Maybe a, Maybe a, Maybe a)
  -> (Maybe a, Maybe a)
discrepants (_, q1, _, q3, _) = (liftA2 (-) q1 aq, liftA2 (+) q3 aq)
  where aq = fmap (* 1.5) $ (-) <$> q3 <*> q1

-- | cleans a non empty list of the discrepant values
cleanDiscrepants :: (Ord a, Fractional a) => [a] -> [a]
cleanDiscrepants xs = if isNothing ci || isNothing cs
  then error "what the fuck"
  else filter (\x ->  x >= fromJust ci && x <= fromJust cs) xs
  where (ci, cs) = discrepants $ fiveNumbers xs
