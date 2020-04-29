module Lesson19 where


import qualified Data.Map                      as Map
import           Data.Maybe
import           Lesson18

data Container
  = Vat Organ
  | Cooler Organ
  | Bag Organ

instance Show Container where
  show (Vat    org) = show org ++ " in a vat"
  show (Cooler org) = show org ++ " in a cooler"
  show (Bag    org) = show org ++ " in a bag"

data Location
  = Lab
  | Kitchen
  | Bathroom
  deriving (Show)

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Brain
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat    a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag    a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process = placeInLocation . organToContainer

report :: (Location, Container) -> String
report (loc, cont) = show cont ++ " in the " ++ show loc

processAndReport :: Maybe Organ -> String
processAndReport (Just organ) = report . process $ organ
processAndReport Nothing      = "error, id not found"

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
  where organ = Map.lookup id catalog

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where getContents id = Map.lookup id catalog

emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers organList = length $ filter isNothing organList

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

maybeMap :: (a -> b) -> [Maybe a] -> [Maybe b]
maybeMap _    []             = []
maybeMap func (Nothing : xs) = Nothing : maybeMap func xs
maybeMap func (Just x  : xs) = Just (func x) : maybeMap func xs

