module Area2
  ( numberOfCombinations
  , expectedValue
  , randomVariableVariance
  , randomVariableStdDeviation
  , binomialProbabilityFunction
  , poissonProbabilityFunction
  , hypgeoProbabilityFunction
  , probabilityOverRange
  , continousMean
  , continousVariance
  , uniformMean
  , uniformVariance
  , uniformLessThan
  , uniformMoreThan
  , exponentialMean
  , exponentialVariance
  , exponentialLessThan
  , exponentialMoreThan
  , transformToStandard
  , exponentialMedian
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
  numberOfCombinations n x * (successChance ** x) * (1 - successChance) ** (n - x)

-- | calculates the probability of an event with @x@ successes,
-- and @lambda@ mean chance of sucess in a poisson distribution
poissonProbabilityFunction :: Double -> Double -> Double
poissonProbabilityFunction x lambda = euler ** (-lambda) * lambda ** x / fact x
  where fact n = product [1 .. n]

-- | Calculates the probability of a hypergeometric function distribition
hypgeoProbabilityFunction
  :: Double -- the number of repetitions of the experiment
  -> Double -- the number seeked (success)
  -> Double -- the size of the total population
  -> Double -- the size of the first sub population
  -> Double -- the size of the second sub population
  -> Double
hypgeoProbabilityFunction repetitions expected population subPopulation1 subPopulation2 =
  ( numberOfCombinations subPopulation1 expected
    * numberOfCombinations subPopulation2 (repetitions - expected)
    )
    / numberOfCombinations population repetitions

-- | Calculates the sums of the probability function @f@ applied to the ranges @(a, b)@, inclusive
probabilityOverRange :: (Double -> Double) -> (Double, Double) -> Double
probabilityOverRange f (a, b) = sum $ map f [a .. b]

-- | Calculates the mean of continous random variable
-- with probabiblity function @f@ on the interval @(a,b)@
continousMean :: (Double -> Double) -> (Double, Double) -> Maybe Double
continousMean f interval = quadRes $ quadRomberg defQuad interval (\x -> f x * x)

-- | Calculates the variance of continous random variable
-- with probabiblity function @f@ on the interval @(a,b)@
continousVariance :: (Double -> Double) -> (Double, Double) -> Maybe Double
continousVariance f interval = liftA2 (-) eX cMean
 where
  cMean = (** 2) <$> continousMean f interval
  eX    = quadRes $ quadRomberg defQuad interval (\x -> f x * x ** 2)

-- | Given the interval @(a, b)@, calculates the mean of a
-- uniform distribution
uniformMean :: (Double, Double) -> Double
uniformMean (alpha, beta) = (alpha + beta) / 2

-- | Given the interval @(a, b)@, calculates the variance of a
-- uniform distribution
uniformVariance :: (Double, Double) -> Double
uniformVariance (alpha, beta) = (beta - alpha) ** 2 / 12

-- | Given the interval @(a, b)@, and a value @x@, calculates the
-- probability of a value be less than @x@, on a uniform distribution
uniformLessThan :: (Double, Double) -> Double -> Double
uniformLessThan (alpha, beta) x = (x - alpha) / (beta - alpha)

-- | Given the interval @(a, b)@, and a value @x@, calculates the
-- probability of a value be more than @x@, on a uniform distribution
uniformMoreThan :: (Double, Double) -> Double -> Double
uniformMoreThan (alpha, beta) x = (beta - x) / (beta - alpha)

-- | Calculates the mean of a exponential distribution given
-- a @lambda value@
exponentialMean :: Double -> Double
exponentialMean = (1 /)

-- | Calculates the variance of a exponential distribution given
-- a @lambda value@
exponentialVariance :: Double -> Double
exponentialVariance lambda = 1 / lambda ** 2

-- | Calculates the median of a exponential distribution given
-- a @lambda value@
exponentialMedian :: Double -> Double
exponentialMedian = (ln 2 /)

-- | Given the @lambda@, and a value @x@, calculates the
-- probability of a value be more than @x@, on a exponential distribution
exponentialMoreThan :: Double -> Double -> Double
exponentialMoreThan lambda x = euler ** (negate lambda * x)

-- | Given the @lambda@, and a value @x@, calculates the
-- probability of a value be less than @x@, on a exponential distribution
exponentialLessThan :: Double -> Double -> Double
exponentialLessThan lambda x = 1 - exponentialMoreThan lambda x

-- | Given @x@, @mean@, and @variance@, calculates the value of 
-- @x@ on a standard normal distribution
transformToStandard :: Double -> Double -> Double -> Double
transformToStandard x mean variance = (x - mean) / variance