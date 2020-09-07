module Roots.FixedPoint
  ( fixedPoint
  )
where

fixedPoint :: (Double -> Double) -> Double -> Int -> Double
fixedPoint _ estimate 0           = estimate
fixedPoint f estimate iteractions = fixedPoint f (f estimate) (iteractions - 1)
