module Prelude
  ( module Relude
  , module Relude.Extra
  , module Relude.Numeric
  , log
  , euler
  )
where
import           Relude
import           Relude.Extra

import           Relude.Numeric

euler :: Double
euler = exp 1

log :: Double -> Double
log = logBase euler
