
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
  | Transparent
  deriving (Show,Eq)

instance Semigroup Color where
  (<>) Transparent a           = a
  (<>) a           Transparent = a
  (<>) Red         Blue        = Purple
  (<>) Blue        Red         = Purple
  (<>) Yellow      Blue        = Green
  (<>) Blue        Yellow      = Green
  (<>) Yellow      Red         = Orange
  (<>) Red         Yellow      = Orange
  (<>) a b | a == b    = a
           | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
           | all (`elem` [Yellow, Blue, Green]) [a, b] = Green
           | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
           | otherwise = Brown

instance Monoid Color where
  mempty = Transparent
  mappend c1 c2 = c1 <> c2

newtype Events = Events { eventsValues :: [String]  }
newtype Probs = Probs {probsValues :: [Double] }

instance Semigroup Events where
  (<>) = combineEvents

instance Semigroup Probs where
  (<>) = combineProbs

instance Monoid Events where
  mempty  = Events []
  mappend = (<>)

instance Monoid Probs where
  mempty  = Probs []
  mappend = (<>)

data PTable = PTable Events Probs


createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where
    rightProbs      = probsValues probs
    totalProbs      = sum rightProbs
    normalizedProbs = Probs $ map (/ totalProbs) rightProbs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = removeLast . mconcat $ pairs
    where
      eventsV = eventsValues events
      probsV  = probsValues probs
      pairs   = zipWith showPair eventsV probsV

removeLast :: (Ord a) => [a] -> [a]
removeLast lst = take almostAll lst where almostAll = length lst - 1

-- this is the cartesian product function directly from the book
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd     = length l2
    repeatedL1 = map (replicate nToAdd) l1
    newL1      = mconcat repeatedL1
    cycledL2   = cycle l2

-- this is my cartesian product function (which looks way more simple)
myProduct :: (a -> b -> c) -> [a] -> [b] -> [c]
myProduct func l1 l2 = map funcToPairs productList
  where
    productList = [ (x, y) | x <- l1, y <- l2 ]
    funcToPairs = uncurry func


combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = Events
  $ myProduct combiner (eventsValues e1) (eventsValues e2)
  where combiner x y = mconcat [x, "-", y]

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = Probs $ myProduct (*) (probsValues p1) (probsValues p2)

instance Semigroup PTable where
  (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
  (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where
      newEvents = combineEvents e1 e2
      newProbs  = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  --mappend pt1 pt2 = pt1 <> pt2

coin :: PTable
coin = createPTable (Events ["heads", "tails"]) (Probs [0.5, 0.5])

spinner :: PTable
spinner =
  createPTable (Events ["red", "blue", "green"]) (Probs [0.1, 0.2, 0.7])


