module Area2
  ( numberOfCombinations
  , expectedValue
  , randomVariableVariance
  , randomVariableStdDeviation
  , binomialProbabilityFunction
  , poissonProbabilityFunction
  )
where

-- | calculates the number of combinations of (n, k), where @n@ is
-- the number of possibilties and @k@ how many picks
-- >>> numberOfCombinations 5 2
-- 10.0
numberOfCombinations :: Double -> Double -> Double
numberOfCombinations n k = fact n / (fact k * fact (n - k))
  where fact x = foldl' (*) 1 [1 .. x]

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
-- repeated @x@ times, with @successChance@ < 1, with binominal
-- distribution
-- >>> binomialProbabilityFunction 3 2 0.6
-- 0.43200000000000005
binomialProbabilityFunction :: Double -> Double -> Double -> Double
binomialProbabilityFunction n x successChance =
  numberOfCombinations n x * (successChance ** x) * (1 - successChance) ** (n - x)

-- | calculates the probability of an event with @x@ successes,
-- and @lambda@ mean chance of sucess in a poisson distribution
poissonProbabilityFunction :: Double -> Double -> Double
poissonProbabilityFunction x lambda = euler ** (-lambda) * lambda ** x / fact x
 where
  euler = exp 1
  fact n = foldl' (*) 1 [1 .. n]
