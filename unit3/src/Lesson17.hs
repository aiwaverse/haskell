module Lesson17 where

import           Data.List
import           Data.Semigroup
data Color
  = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  deriving (Show,Eq)

instance Semigroup Color where
  (<>) Red    Blue   = Purple
  (<>) Blue   Red    = Purple
  (<>) Yellow Blue   = Green
  (<>) Blue   Yellow = Green
  (<>) Yellow Red    = Orange
  (<>) Red    Yellow = Orange
  (<>) a b | a == b    = a
           | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
           | all (`elem` [Yellow, Blue, Green]) [a, b] = Green
           | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
           | otherwise = Brown


type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
 where
  totalProbs      = sum probs
  normalizedProbs = map (/ totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = removeLast . mconcat $ pairs
    where pairs = zipWith showPair events probs

removeLast :: (Ord a) => [a] -> [a]
removeLast lst = take almostAll lst where almostAll = length lst - 1

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
 where
  nToAdd     = length l2
  repeatedL1 = map (replicate nToAdd) l1
  newL1      = mconcat repeatedL1
  cycledL2   = cycle l2

myProduct :: (a -> b -> c) -> [a] -> [b] -> [c]
myProduct func l1 l2 = map funcToPairs productList
 where
  productList = [ (x, y) | x <- l1, y <- l2 ]
  funcToPairs = uncurry func


combineEvents :: Events -> Events -> Events
combineEvents = myProduct combiner where combiner x y = mconcat [x, "-", y]

combineProbs :: Probs -> Probs -> Probs
combineProbs = myProduct (*)

instance Semigroup PTable where
  (<>) ptable1        (PTable [] []) = ptable1
  (<>) (PTable [] []) ptable2        = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
   where
    newEvents = combineEvents e1 e2
    newProbs  = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []
  mappend pt1 pt2 = pt1 <> pt2

coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]


