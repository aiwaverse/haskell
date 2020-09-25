module Prelude
  ( module Relude
  , euler
  , ln
  )
where

import           Relude

euler :: Double
euler = exp 1

ln :: (Double -> Double)
ln = logBase euler