{- |
Copyright: (c) 2020 Aiwa
SPDX-License-Identifier: MIT
Maintainer: Aiwa <aiwavision@protonmail.com>

See README for more info
-}

module Utilities
    ( relativeError
    , digse
    )
where

-- | calculates the relative error of @x@ and @xBar@
relativeError :: Fractional a => a -> a -> a
relativeError x xBar = abs $ (x - xBar) / abs x

-- | calculates the number of significative correct digits of @x@ and @xBar@
digse :: (Integral t, Fractional a, Ord a) => a -> a -> t
digse x xBar = internal x xBar 0
  where
    internal x' xBar' s =
        if op >= (5 * 10 ^^ (-s)) then s - 1 else internal x' xBar' (s + 1)
    op = abs (x - xBar) / abs x
