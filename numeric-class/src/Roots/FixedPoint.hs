module Roots.FixedPoint
  ( fixedPoint
  )
where

-- | Given a function @f@, an @estimate@ and the maximum @iteractions@
-- Returns an approximate point where f(estimate) = estimate
fixedPoint :: (Double -> Double) -> Double -> Int -> Double
fixedPoint _ estimate 0           = estimate
fixedPoint f estimate iteractions = fixedPoint f (f estimate) (iteractions - 1)
