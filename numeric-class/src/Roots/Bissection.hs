module Roots.Bissection
  ( bissection
  )
where

-- | Calculates the root of a function @f@ given two extremes @a@ and @b@
-- a @tolerance and an "iteration" limit @n (it's recursive)
bissection :: (Double -> Double) -> Double -> Double -> Double -> Int -> Double
bissection _ _ _ _ 0 = error "Number of iterations exceeded"
bissection f a b tolerance n | f p == 0 = p
                             | -- a perfect root
                               (b - a) / 2 < tolerance = p
                             | -- the difference between the extremes is small enough
                               (f a * f p) > 0 = bissection f p b tolerance (n - 1)
                             | -- test to see if p goes to a
                               otherwise = bissection f a p tolerance (n - 1)
                               -- or b
  where p = a + (b - a) / 2
