-- |
-- Copyright: (c) 2020 Aiwa
-- SPDX-License-Identifier: MIT
-- Maintainer: Aiwa <aiwavision@protonmail.com>
--
-- See README for more info
module Utilities
  ( relativeError
  , digse
  , babilonianRoot
  , printIncreasingIteractions
  , format
  , format25
  )
where

import           Numeric
import           Data.Foldable(traverse_)

-- | calculates the relative error of @x@ and @xBar@
relativeError :: Fractional a => a -> a -> a
relativeError x xBar = abs $ (x - xBar) / abs x

-- | calculates the number of significative correct digits of @x@ and @xBar@
digse :: (Integral t, Fractional a, Ord a) => a -> a -> t
digse x xBar = internal x xBar 0
 where
  internal x' xBar' s =
    if op >= (5 * 10 ^^ (-s)) then s - 1 else internal x' xBar' (s + 1)
  op = abs (x - xBar) / abs x

-- | Given a @number@ to calcualte the root of, an @estimate@ and the
-- maximum number of @iteractions@, returns an approximate root
babilonianRoot :: Double -> Double -> Int -> Double
babilonianRoot _ estimate 0 = estimate
babilonianRoot number estimate iteractions =
  babilonianRoot number (estimate / 2 + number / (2 * estimate)) (iteractions - 1)

-- | Given a function @f@ and two numbers @a@ and @b@ as a range
-- prints de function being applied to each number in the range
-- This is highly innefective, but since these things have usually
-- a small range and aren't expesnive, it's not a problem
printIncreasingIteractions :: Show a => (Int -> a) -> Int -> Int -> IO ()
printIncreasingIteractions f a b = traverse_ (print . f) [a .. b]

-- | Given a number of @digits@ and a @RealFloat@ @num@, return a @String@ with
-- is made of @num@ with the number of @digits@ after the decimal point 
format :: RealFloat a => Int -> a -> String
format digits num = showFFloat (Just digits) num ""

-- | Shortcut for @format 25@ (useful for the classes, for some reason)
format25 :: RealFloat a => a -> String
format25 = format 25
