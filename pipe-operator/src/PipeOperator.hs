{- |
Copyright: (c) 2020 Aiwa
SPDX-License-Identifier: MIT
Maintainer: Aiwa <aiwavision@protonmail.com>

See README for more info
-}

module PipeOperator
    ( (|>)
    , (<|)
    )
where

-- | This is a pipe operator like F#'s, works pretty much the same I imagine, behaves like (&)
infixl 0 |>
(|>) :: a -> (a -> b) -> b
a |> f = f a

-- | The backwards pipe operator
infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| a = f a
