{- |
Copyright: (c) 2020 Aiwa
SPDX-License-Identifier: MIT
Maintainer: Aiwa <aiwavision@protonmail.com>

See README for more info
-}

module NumericClass where

-- | calculates the relative error of @x@ and @xBar@
relativeError :: Fractional a => a -> a -> a
relativeError x xBar = abs (x - xBar) / abs x

-- | calculates the number of significative correct digits of @x@ and @xBar@
digse :: (RealFrac a, Integral c, Floating a) => a -> a -> c
digse x xBar = round . abs . logBase 10 $ relativeError x xBar
