module Roots.Bissection
    ( bissection
    )
where

-- | Calculates the root of a function @f@ given two extremes @a@ and @b@
-- a @tolerance and an "iteration" limit @n (it's recursive)

bissection :: (Double -> Double) -> Double -> Double -> Double -> Int -> Double
bissection _ _ _ _ 0 = error "Number of iterations exceeded"
bissection f a b tolerance n | f p == 0 = p
                             | (b - a) / 2 < tolerance = p
                             | (f a * f p) > 0 = bissection f p b tolerance (n - 1)
                             | otherwise = bissection f a p tolerance (n - 1)
    where p = a + (b - a) / 2
