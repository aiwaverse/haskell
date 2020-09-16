module Area2
  ( numberOfCombinations
  , expectedValue
  , randomVariableVariance
  , randomVariableStdDeviation
  , probabilityFunction
  )
where

import           Data.List                      ( foldl1' )

-- | calculates the number of combinations of (n, k), where @n@ is
-- the number of possibilties and @k@ how many picks
-- >>> numberOfCombinations 5 2
-- 10.0
numberOfCombinations :: Fractional a => Int -> Int -> a
numberOfCombinations n k = fact n / (fact k * fact (n - k))
  where fact x = fromIntegral $ foldl1' (*) [1 .. x]

-- | calculates the expected value, or ponderate mean, given a list of
-- values and their probabilities, which must sum to 1
-- >>> expectedValue [(0,0.1),(1,0.6),(2,0.3)]
-- 1.2
expectedValue :: Num a => [(a, a)] -> a
expectedValue = sum . map (uncurry (*))

-- | calculates the variance of a random variable, given a list of
-- values and their probabilities, which must sum to 1
-- >>> randomVariableVariance [(0,0.1),(1,0.6),(2,0.3)]
-- 0.36
randomVariableVariance :: Num a => [(a, a)] -> a
randomVariableVariance xs = sum . map (uncurry (\x p -> (x - u) ^ 2 * p)) $ xs
  where u = expectedValue xs

-- | calculates the standard deviation of a random variable, given a list of
-- values and their probabilities, which must sum to 1
-- >>> randomVariableStdDeviation [(0,0.1),(1,0.6),(2,0.3)]
-- 0.6
randomVariableStdDeviation :: Floating a => [(a, a)] -> a
randomVariableStdDeviation = sqrt . randomVariableVariance

-- | calculates the probability of an event of @n@ possibilities, 
-- repeated @x@ times, with @successChance@ < 1
-- >>> probabilityFunction 3 2 0.6
-- 0.43200000000000005
-- >>> probabilityFunction 3 1 0.6
-- 0.28800000000000003
probabilityFunction :: Int -> Int -> Double -> Double
probabilityFunction n x successChance =
  numberOfCombinations n x * successChance ^ x * (1 - successChance) ^ (n - x)
