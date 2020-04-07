module Ranges (createRange, inRange, rangeString, roundTo) where


createRange :: [Double] -> Double -> Integer -> [(Double, Double)]
createRange _ _ 0 = []
createRange (x : xs) interval n =
  (roundedX, roundedXinterval) : createRange (roundedXinterval : xs) interval (n - 1)
  where
    roundedX = roundTo x 1
    roundedXinterval = flip roundTo 1 $ roundedX + interval

inRange :: (Num a, Ord a) => [a] -> (a, a) -> [a]
list `inRange` (lo, hi) = dropWhile (< lo) . takeWhile (<= hi) $ list

rangeString :: [(Double, Double)] -> [String]
rangeString []       = []
rangeString (x : xs) = mconcat [show lo, " |-> ", show hi] : rangeString xs
  where
    lo = fst x
    hi = snd x

-- |The 'roundTo' function rounds a RealFrac to n decimal digits
roundTo :: (RealFrac a) => a      -- RealFrac to round
                        -> Int    -- Number of decimal digits
                        -> a
roundTo number digits =  fromInteger (round $ number * (10 ^ digits)) / (10.0 ^^ digits)
