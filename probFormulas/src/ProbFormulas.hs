{- |
Copyright: (c) 2020 Aiwa
SPDX-License-Identifier: MIT
Maintainer: Aiwa <aiwavision@protonmail.com>

See README for more info
-}

module ProbFormulas
    ( mean
    , variance
    , tValue
    , tValueComparasion
    , tValuePaired
    , difference
    )
where

-- | mean calculates the arithimetic mean of a list of Fractionals
mean :: Fractional a => [a] -> a
mean l = uncurry (/) $ foldl' (\(a, n) ax -> (a + ax, n + 1)) (0, 0) l

-- | variance calculates the variance of a list of Fractionals
variance :: Fractional a => [a] -> a
variance l = sum $ map (\x -> (x - lMean) ^ (2 :: Integer) / n) l
  where
    n     = fromIntegral $ length l - 1
    lMean = mean l

-- | calculates the standard deviation (sqrt of variance) of a list of Floating
standardDeviation :: Floating a => [a] -> a
standardDeviation = sqrt . variance

-- | Calculates the t value of a (mean, sd, group size) in relation to a μ0
tValue :: (Double, Double, Int) -> Double -> Double
tValue (x, s, n) mi = (x - mi) / (s / sqrt floatN) where floatN = fromIntegral n

-- | Calculates the t value of two sets of (mean, sd, group size)
tValueComparasion :: (Double, Double, Int) -> (Double, Double, Int) -> Double
tValueComparasion (x1, s1, n1) (x2, s2, n2) = (x1 - x2) / dividend
  where
    floatN1   = fromIntegral n1
    floatN2   = fromIntegral n2
    cVariance = combinedVariance (s1, n1) (s2, n2)
    dividend  = sqrt . (* cVariance) $ (1 / floatN1) + (1 / floatN2)

-- | calculates the combined variance of (sd1, group size 1) (sd2, group size 2)
combinedVariance :: (Double, Int) -> (Double, Int) -> Double
combinedVariance (s1, n1) (s2, n2) = denomin / dividend
  where
    floatN1  = fromIntegral n1
    floatN2  = fromIntegral n2
    denomin  = s1 ^ (2 :: Integer) * (floatN1 - 1) + s2 ^ (2 :: Integer) * (floatN2 - 1)
    dividend = (floatN1 - 1) + (floatN2 - 1)

-- | quickly calculates the absolute difference of all elements of two lists of double
difference :: [Double] -> [Double] -> [Double]
difference = zipWith (\x y -> abs (x - y))

-- | calculates the t value of a list, given the least is the difference of two samples
tValuePaired :: [Double] -> Double
tValuePaired l = mean l / (standardDeviation l / sqrt floatLenght)
    where floatLenght = fromIntegral . length $ l
