module Area2
  ( numberOfCombinations
  , expectedValue
  , randomVariableVariance
  , randomVariableStdDeviation
  , binomialProbabilityFunction
  , poissonProbabilityFunction
  , continousMean
  , continousVariance
  )
where

import           Numeric.Tools.Integration

-- | calculates the number of combinations of (n, k), where @n@ is
-- the number of possibilties and @k@ how many picks
-- >>> numberOfCombinations 5 2
-- 10.0
numberOfCombinations :: Double -> Double -> Double
numberOfCombinations n k = fact n / (fact k * fact (n - k))
  where fact x = product [1 .. x]

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
randomVariableVariance xs = sum . map (\(x, p) -> (x - u) ^ 2 * p) $ xs
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
  numberOfCombinations n x
    *  (successChance ** x)
    *  (1 - successChance)
    ** (n - x)

-- | calculates the probability of an event with @x@ successes,
-- and @lambda@ mean chance of sucess in a poisson distribution
poissonProbabilityFunction :: Double -> Double -> Double
poissonProbabilityFunction x lambda = euler ** (-lambda) * lambda ** x / fact x
 where
  euler = exp 1
  fact n = product [1 .. n]

continousMean :: (Double -> Double) -> (Double, Double) -> Maybe Double
continousMean f interval =
  quadRes $ quadRomberg defQuad interval (\x -> f x * x)

continousVariance :: (Double -> Double) -> (Double, Double) -> Maybe Double
continousVariance f interval = liftA2 (-) eX cMean
 where
  cMean = (** 2) <$> continousMean f interval
  eX    = quadRes $ quadRomberg defQuad interval (\x -> f x * x ** 2)
