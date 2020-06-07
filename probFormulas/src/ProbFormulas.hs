{- |
Copyright: (c) 2020 Aiwa
SPDX-License-Identifier: MIT
Maintainer: Aiwa <aiwavision@protonmail.com>

See README for more info
-}

module ProbFormulas
    ( mean
    , variance
    , tVariance
    , tResult
    , tPairs
    , difference
    )
where

mean :: [Double] -> Double
mean = meanFold

meanFold :: Fractional a => [a] -> a
meanFold l = uncurry (/) $ foldl' (\(a, n) ax -> (a + ax, n + 1)) (0, 0) l

variance :: [Double] -> Double
variance l = sum $ map (\x -> (x - lMean) ^ (2 :: Integer) / n) l
  where
    n     = fromIntegral $ length l - 1
    lMean = meanFold l

standardDeviation :: [Double] -> Double
standardDeviation = sqrt . variance

tVariance :: (Double, Double, Int) -> Double -> Double
tVariance (x, s, n) mi = (x - mi) / (s / sqrt floatN) where floatN = fromIntegral n

tResult :: (Double, Double, Int) -> (Double, Double, Int) -> Double
tResult (x1, s1, n1) (x2, s2, n2) = (x1 - x2) / dividend
  where
    floatN1   = fromIntegral n1
    floatN2   = fromIntegral n2
    cVariance = combinedVariance (s1, n1) (s2, n2)
    dividend  = sqrt . (* cVariance) $ (1 / floatN1) + (1 / floatN2)

combinedVariance :: (Double, Int) -> (Double, Int) -> Double
combinedVariance (s1, n1) (s2, n2) = denomin / dividend
  where
    floatN1  = fromIntegral n1
    floatN2  = fromIntegral n2
    denomin  = s1 ^ (2 :: Integer) * (floatN1 - 1) + s2 ^ (2 :: Integer) * (floatN2 - 1)
    dividend = (floatN1 - 1) + (floatN2 - 1)

difference :: [Double] -> [Double] -> [Double]
difference = zipWith (\x y -> abs (x - y))

tPairs :: [Double] -> Double
tPairs l = meanFold l / (standardDeviation l / sqrt floatLenght)
    where floatLenght = fromIntegral . length $ l
