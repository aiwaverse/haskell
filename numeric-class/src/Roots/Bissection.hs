module Roots.Bissection
  ( bissection
  , bissectionIteractions
  )
where

-- | Calculates the root of a function @f@ given two extremes @a@ and @b@
-- a @tolerance and an "iteration" limit @n@ (it's recursive)
bissection :: (Double -> Double) -> Double -> Double -> Double -> Int -> Either String Double
bissection _ _ _ _ 0 = Left "Number of iterations exceeded"
bissection f a b tolerance n | f p == 0 = Right p
                             | -- a perfect root
                               (b - a) / 2 < tolerance = Right p
                             | -- the difference between the extremes is small enough
                               (f a * f p) > 0 = bissection f p b tolerance (n - 1)
                             | -- test to see if p goes to a
                               otherwise = bissection f a p tolerance (n - 1)
                               -- or b
  where p = a + (b - a) / 2

-- | given @(a,b)@ and a @stop@ criteria, calculates how many bissection iteractions
-- will be needed to reach the desire absolute error
-- function mainly derived empirically
bissectionIteractions :: (Double, Double) -> Double -> Int
bissectionIteractions (a, b) stop = findStopPoint (b - a) stop 0
 where
  findStopPoint err tol n =
    if err < tol then n - 1 else findStopPoint (err / 2) tol (n + 1)
