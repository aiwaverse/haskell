{-# LANGUAGE TypeApplications #-}
module Conversions
  ( numberToDigits
  , digitsToNumber
  , digitsToNumberB
  , baseConverterInt
  , baseConverterDec
  , baseConvert
  , roundTo
  )
where

import           Data.List

-- to deal with the zero input
numberToDigitsI :: Integer -> Integer -> [Integer]
numberToDigitsI _ 0 = []
numberToDigitsI base number =
  number `mod` base : numberToDigitsI base (number `div` base)

numberToDigits :: Integer -> Integer -> [Integer]
numberToDigits _    0      = [0]
numberToDigits base number = numberToDigitsI base number

-- generalizes the idea, this can convert any list of numbers to it's decimal equivalent given the base the number is in
-- actually an accident
digitsToNumberB :: Integer -> [Integer] -> Integer
digitsToNumberB base digits =
  sum [ x * base ^ n | (x, n) <- zip digits [0 ..] ]

-- if used with 10, it just converts the number for our reading eyes
digitsToNumber :: [Integer] -> Integer
digitsToNumber = digitsToNumberB 10

baseConverterInt :: Integer -> Integer -> Integer
baseConverterInt base = digitsToNumber . numberToDigits base

baseConverterDec :: Integer -> Double -> Integer
baseConverterDec base number = digitsToNumber $ baseConverterDecI base number

baseConverterDecI :: Integer -> Double -> [Integer]
baseConverterDecI base number = case rest of
  0 -> [whole]
  _ -> whole : baseConverterDecI base rest
  where
    doubled = roundTo (number * fromIntegral base) 2
    whole   = floor doubled
    rest    = doubled - fromIntegral whole


baseConvert :: Integer -> String -> String
baseConvert base number = show convertedInt ++ "." ++ show convertedDec
  where
    (int, dec) =
      (\(i, d) -> (read @Integer i, read @Double $ '0' : d))
        $ span (/= '.') number
    convertedInt = baseConverterInt base int
    convertedDec = baseConverterDec base dec

roundTo :: (RealFrac a) => a -> Int -> a
roundTo number digits =
  fromInteger (round $ number * (10 ^ digits)) / (10.0 ^^ digits)
