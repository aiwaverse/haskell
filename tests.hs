{-# LANGUAGE FlexibleContexts #-}

isInFirstHalf :: (Ord a) => a -> [a] -> Bool
isInFirstHalf el list = el `elem` firstHalf
    where firstHalf = take (length list `div` 2) list

euclidsAlgo :: Integer -> Integer -> Integer
euclidsAlgo a b | a `mod` b == 0 = b
                | otherwise      = euclidsAlgo newA newB
  where
    newA = b
    newB = a `mod` b

-- patter-matching version
euclidsAlgoPm :: Integer -> Integer -> Integer
euclidsAlgoPm a 0 = a
euclidsAlgoPm a b = euclidsAlgoPm b (a `mod` b)

sayAmount :: Integer -> String
sayAmount n = case n of
    1 -> "One"
    2 -> "Two"
    _ -> "a bunch"

myTail :: (Ord a) => [a] -> [a]
myTail []       = []
myTail (x : xs) = xs

myDrop :: (Ord a) => Integer -> [a] -> [a]
myDrop n []  = []
myDrop 0 lst = lst
myDrop n lst = myDrop (n - 1) $ tail lst

myLength :: (Ord a) => [a] -> Integer
-- how? (btw eta reduce)
myLength = foldr (const (+ 1)) 0

myOtherLength :: (Ord a) => [a] -> Integer
myOtherLength lst = sum $ map (const 1) lst

-- this is very slow
ackermann :: (Eq a, Num a) => a -> a -> a
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) $ ackermann m (n - 1)

collatz :: Integer -> Integer
collatz 1 = 1
collatz n | even n    = 1 + collatz (n `div` 2)
          | otherwise = 1 + collatz (3 * n + 1)

myReverse :: (Ord a) => [a] -> [a]
myReverse [] = []
myReverse list = last list : myReverse (take (length list - 1) list)