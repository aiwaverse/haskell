module Roots.Newton
  ( newton
  )
where

newton :: (Double -> Double) -> (Double -> Double) -> Double -> Int -> Double
newton _ _ estimate 0 = estimate
newton f derivative estimate iteractions =
  newton f derivative (estimate - f estimate / derivative estimate) (iteractions - 1)
