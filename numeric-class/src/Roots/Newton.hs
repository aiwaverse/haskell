module Roots.Newton
  ( newton
  )
where

-- | given @f@ and it's @derivative@, plus an @estimate@ and an @iteractions@ number
-- calculates a root of @f@
newton :: (Double -> Double) -> (Double -> Double) -> Double -> Int -> Double
newton _ _ estimate 0 = estimate
newton f derivative estimate iteractions =
  newton f derivative (estimate - f estimate / derivative estimate) (iteractions - 1)
