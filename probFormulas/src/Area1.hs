 {-# LANGUAGE ViewPatterns #-}
-- while redundant, this makes stylish haskell work
module Area1
  ( combinations
  , fiveNumbers
  , discrepants
  , cleanDiscrepants
  , countOccurrences
  , median
  , firstQuartile
  , thirdQuartile
  , meanDeviation
  , variationCoefficient
  , variationCoefficientList
  )
where

import           Area3                          ( mean
                                                , standardDeviation
                                                )

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
  then meanV (xs !!? ceiling pOdd, xs !!? floor pOdd)
  else meanV (xs !!? ceiling pEven, xs !!? floor pEven)
 where
  pOdd  = ((genericLength xs + 1) / 4) - 1
  pEven = ((genericLength xs + 2) / 4) - 1
  meanV (Just x , Just y ) = Just ((x + y) / 2)
  meanV (Nothing, _      ) = Nothing
  meanV (_      , Nothing) = Nothing


thirdQuartile :: (Ord a, Fractional a) => [a] -> Maybe a
thirdQuartile (sort -> xs) = if odd (length xs)
  then meanV (xs !!? ceiling pOdd, xs !!? floor pOdd)
  else meanV (xs !!? ceiling pEven, xs !!? floor pEven)
 where
  pOdd  = (3 * (genericLength xs + 1) / 4) - 1
  pEven = ((3 * genericLength xs + 2) / 4) - 1
  meanV (Just x , Just y ) = Just ((x + y) / 2)
  meanV (Nothing, _      ) = Nothing
  meanV (_      , Nothing) = Nothing


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
cleanDiscrepants xs = if isNothing (fst discre) || isNothing (snd discre)
  then error "what the fuck"
  else filter (\x -> x >= ci && x <= cs) xs
  where discre@(Just ci, Just cs) = discrepants $ fiveNumbers xs

-- | calculates the mean deviation of a list @xs@ of Doubles
meanDeviation :: [Double] -> Double
meanDeviation xs = sumOfDiff / genericLength xs
 where
  m         = mean xs
  sumOfDiff = sum $ map (\x -> abs $ x - m) xs

-- | Calculates the coefficient of variation given a @standard deviation@
-- and a @mean@
variationCoefficient :: Double -> Double -> Double
variationCoefficient stdDeviation meanValue = stdDeviation / meanValue

-- | Calculates the coefficient of variation given a list of values @xs@
variationCoefficientList :: [Double] -> Double
variationCoefficientList xs = variationCoefficient (standardDeviation xs) (mean xs)
