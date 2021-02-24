{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{- |
Copyright: (c) 2020 Aiwa
SPDX-License-Identifier: MIT
Maintainer: Aiwa <aiwavision@protonmail.com>

See README for more info
-}

module Funsets
  ( FunSet
  , contains
  , singletonSet
  , union
  , intersect
  , diff
  , setFilter
  , forall
  , exists
  , setMap
  )
where

bound :: Int
bound = 1000

type FunSet = Int -> Bool

instance Show FunSet where
  show f = "{" ++ setValues ++ "}"
   where
    setValues = intercalate ", "
      $ [show @String x | x <- [(-bound) .. bound], f `contains` x ]

-- | Returns True if a given FunSet contains @n@, False otherwise
contains :: FunSet -> Int -> Bool
contains = ($)

-- | Returns a FunSet of a single element @n@
singletonSet :: Int -> FunSet
singletonSet n = (== n)

-- | Returns a FunSet that is the logical union of @s1@ and @s2@
union :: FunSet -> FunSet -> FunSet
union s1 s2 x = s1 `contains` x || s2 `contains` x

-- | Returns a FunSet that is the logical disjunction of @s1@ and @s2@
intersect :: FunSet -> FunSet -> FunSet
intersect s1 s2 x = s1 `contains` x && s2 `contains` x

-- | Returns a Funset that every element of @s1@ is not on @s2@
diff :: FunSet -> FunSet -> FunSet
diff s1 s2 x = s1 `contains` x && not (s2 `contains` x)

-- | Returns all elements of @s@ to which a condition @p@ holds
setFilter :: FunSet -> (Int -> Bool) -> FunSet
setFilter = intersect

-- | Returns True if all elements of @s@ satisfy the condition @p@, false otherwise
forall :: FunSet -> (Int -> Bool) -> Bool
forall set condition =
  all (\x -> not (set `contains` x) || condition x) [(-bound) .. bound]

-- | Return True if there's at least one element of @s@ that satisfies @p@
-- Can be written with predicate logic using forall, but I find it less readable
exists :: FunSet -> (Int -> Bool) -> Bool
exists set condition =
  any (\x -> set `contains` x && condition x) [(-bound) .. bound]

-- | maps a function to a FunSet
setMap :: FunSet -> (Int -> Int) -> FunSet
setMap s f x = exists s (\y -> x == f y)

