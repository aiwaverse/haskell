module Lesson18 where

import qualified Data.Map                      as Map

newtype Box a = Box a deriving (Show)

wrap :: a -> Box a
wrap = Box

unwrap :: Box a -> a
unwrap (Box x) = x

data Triple a = Triple a a a deriving (Show)

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x


-- think Bone isn't an organ but who cares
data Organ = Heart | Brain | Kidney | Spleen | Bone deriving (Show, Eq, Ord, Enum)

-- missed Bone on Purpose
allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

count :: (Ord a) => [a] -> a -> Int
count l cmp =  length $ filter (== cmp) l

organToCount :: [(Organ,Int)]
organToCount = zip allOrgans organCount 
  where organCount = map (count organs) allOrgans

organInventory :: Map.Map Organ Int
organInventory = Map.fromList organToCount

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

boxMap :: (a -> b) -> Box a -> Box b
boxMap func = wrap . func . unwrap

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap func t = Triple newF newS newT
  where
    newF = func . first $ t
    newS = func . second $ t
    newT = func . third $ t

type Point3D = Triple Double
aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3
