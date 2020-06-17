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
    , tValueComparasionDifferent
    , tValuePaired
    , difference
    , qValue
    , zValue
    , zValueComparasion
    , combFreedomDregess
    , linearPersonCoefficient
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

-- | Calculates the t value of two sets of (mean, sd, group size) if the variances are different
tValueComparasionDifferent :: (Double, Double, Int) -> (Double, Double, Int) -> Double
tValueComparasionDifferent (x1, s1, n1) (x2, s2, n2) =
    ((x1 - x2) /) . sqrt $ f s1 n1 + f s2 n2
    where f s n = s ** 2 / fromIntegral n

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

-- | calculates the q value given (variance, group size) -> variance to be compared (σ)
qValue :: (Double, Int) -> Double -> Double
qValue (s², n) σ² = (/ σ²) . (* s²) . fromIntegral $ (n - 1)

-- | calculates the z value given (affected group, group size) -> proportion to be compared (π)
zValue :: (Double, Int) -> Double -> Double
zValue (x, n) π = ((p - π) /) . sqrt . (/ fromIntegral n) $ π * (1 - π)
    where p = x / fromIntegral n

-- | calculates the z value (proportion) given two pairs of (affected group, group size)
zValueComparasion :: (Double, Int) -> (Double, Int) -> Double
zValueComparasion (x1, n1) (x2, n2) = ((p1 - p2) /) . sqrt $ f p1 n1 + f p2 n2
  where
    p1 = x1 / fromIntegral n1
    p2 = x2 / fromIntegral n2
    f p n = (/ fromIntegral n) . (* p) $ (1 - p)

-- | calculates the combined freedom degrees of (s1, n1) (s2, n2)
combFreedomDregess :: (Double, Int) -> (Double, Int) -> Int
combFreedomDregess (s1, n1) (s2, n2) =
    round
        . (/ (f s1 n1 + f s2 n2))
        . (** 2)
        $ (s1 ** 2 / fromIntegral n1)
        + (s2 ** 2 / fromIntegral n2)
    where f s n = (s ** 2 / fromIntegral n) ** 2 / (fromIntegral n - 1)

linearPersonCoefficient :: Floating a => [a] -> [a] -> Maybe a
linearPersonCoefficient x@(length -> n1) y@(length -> n2) =
    if n1 /= n2 then Nothing else Just $ sxx x * sxx y & sqrt & (sxy x y /)

-- | function to use in the pearson linear coefficient
sxx :: Floating a => [a] -> a
sxx x = (sumOfSquares -) . (/ (fromIntegral . length $ x)) $ squareOfSums
  where
    sumOfSquares = sum $ map (** 2) x
    squareOfSums = (** 2) $ sum x

-- | function to use in the pearson linear coefficient
sxy :: Floating a => [a] -> [a] -> a
sxy x y = (sumOfProd -) . (/ (fromIntegral . length $ x)) $ sum x * sum y
    where sumOfProd = sum $ zipWith (*) x y
