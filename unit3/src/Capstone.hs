module Capstone where

import           Data.List
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Semigroup
import           Lesson17                       ( removeLast )

file1 :: [(Int, Double)]
file1 =
  [ (1 , 200.1)
  , (2 , 199.5)
  , (3 , 199.4)
  , (4 , 198.9)
  , (5 , 199.0)
  , (6 , 200.2)
  , (9 , 200.3)
  , (10, 201.2)
  , (12, 202.9)
  ]
file2 :: [(Int, Double)]
file2 =
  [ (11, 201.6)
  , (12, 201.5)
  , (13, 201.5)
  , (14, 203.5)
  , (15, 204.9)
  , (16, 207.1)
  , (18, 210.5)
  , (20, 208.8)
  ]
file3 :: [(Int, Double)]
file3 =
  [ (10, 201.2)
  , (11, 201.6)
  , (12, 201.5)
  , (13, 201.5)
  , (14, 203.5)
  , (17, 210.5)
  , (24, 215.1)
  , (25, 218.7)
  ]
file4 :: [(Int, Double)]
file4 =
  [ (26, 219.8)
  , (27, 220.5)
  , (28, 223.8)
  , (29, 222.8)
  , (30, 223.8)
  , (31, 221.7)
  , (32, 222.3)
  , (33, 220.8)
  , (34, 219.4)
  , (35, 220.1)
  , (36, 220.6)
  ]

data TS a = TS [Int] [Maybe a]

insertMaybePair :: Ord k => (k, Maybe v) -> Map.Map k v -> Map.Map k v
insertMaybePair (_  , Nothing   ) myMap = myMap
insertMaybePair (key, Just value) myMap = Map.insert key value myMap

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2        = ts2
combineTS ts1        (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) =
  let bothTimes      = mconcat [t1, t2]
      completeTimes  = [minimum bothTimes .. maximum bothTimes]
      tvMap          = foldr insertMaybePair Map.empty (zip t1 v1)
      updatedMap     = foldr insertMaybePair tvMap (zip t2 v2)
      combinedValues = map (`Map.lookup` updatedMap) completeTimes
  in  TS completeTimes combinedValues

instance Semigroup (TS a) where
  (<>) = combineTS

instance Monoid (TS a) where
  mempty  = TS [] []
  mappend = (<>)

instance Show a => Show (TS a) where
  show (TS times values) = removeLast . mconcat $ rows
    where rows = zipWith showTVPair times values

createTS :: [Int] -> [a] -> TS a
createTS times values =
  let completeTimes  = [minimum times .. maximum times]
      timeValueMap   = Map.fromList (zip times values)
      extendedValues = map (`Map.lookup` timeValueMap) completeTimes
  in  TS completeTimes extendedValues

fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs = createTS times values
  where (times, values) = unzip tvPairs

showTVPair :: Show a => Int -> Maybe a -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing      = mconcat [show time, "|NA\n"]

ts1 :: TS Double
ts1 = fileToTS file1
ts2 :: TS Double
ts2 = fileToTS file2
ts3 :: TS Double
ts3 = fileToTS file3
ts4 :: TS Double
ts4 = fileToTS file4
tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

mean :: (Real a) => [a] -> Double
mean xs = listSum / size
  where
    listSum = realToFrac . sum $ xs
    size    = realToFrac . length $ xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) | all isNothing values = Nothing
                         | otherwise            = Just tsMean
  where
    justList = filter isJust values
    tsMean   = mean $ map fromJust justList

type CompareFunc a = a -> a -> a
type CompareIntMaybe a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

compareIntMaybeTuple :: Eq a => CompareFunc a -> CompareIntMaybe a
compareIntMaybeTuple _        (i1, Nothing  ) (i2, Nothing  ) = (i1, Nothing)
compareIntMaybeTuple _        (_ , Nothing  ) (i2, val      ) = (i2, val)
compareIntMaybeTuple _        (i1, val      ) (_ , Nothing  ) = (i1, val)
compareIntMaybeTuple compFunc (i1, Just val1) (i2, Just val2) = result
  where
    result | compFunc val1 val2 == val1 = (i1, Just val1)
           | otherwise                  = (i2, Just val2)

compareTS :: Eq a => CompareFunc a -> TS a -> Maybe (Int, Maybe a)
compareTS _ (TS [] []) = Nothing
compareTS func (TS times vals) | all isNothing vals = Nothing
                               | otherwise          = Just best
  where
    pairs = zip times vals
    best  = foldr (compareIntMaybeTuple func) (0, Nothing) pairs

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair _        Nothing  = Nothing
diffPair Nothing  _        = Nothing
diffPair (Just x) (Just y) = Just (x - y)

diffTS :: Num a => TS a -> TS a
diffTS (TS []    []    ) = TS [] []
diffTS (TS times values) = TS times (Nothing : diffValues)
  where
    shiftValues = tail values
    diffValues  = zipWith diffPair shiftValues values
