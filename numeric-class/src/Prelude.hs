module Prelude
  ( module Relude
  , module Relude.Extra
  , module Relude.Numeric
  , ln
  , euler
  )
where
import           Relude
import           Relude.Extra

import           Relude.Numeric

euler :: Double
euler = exp 1

ln :: Double -> Double
ln = logBase euler
