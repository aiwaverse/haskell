module Lesson21 where

data Women 
  = Lesbian
  | Bissexual { preferece :: Double }
  | Straight

data Pizza = Pizza { size :: Double
                   , cost :: Double
                   }

costPerCm :: Pizza -> Double
costPerCm p = cost p / (areaGivenDiameter . size $ p)

areaGivenDiameter :: Double -> Double
areaGivenDiameter diam = pi * radius ^ 2 where radius = diam / 2

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 | costP1 < costP2 = p1
                    | otherwise       = p2
  where
    costP1 = costPerCm p1
    costP2 = costPerCm p2

describePizza :: Pizza -> String
describePizza pizza@(Pizza s c) = mconcat
  [ "The "
  , show s
  , " pizza is cheaper at "
  , show $ costPerCm pizza
  , " per square cm"
  ]
