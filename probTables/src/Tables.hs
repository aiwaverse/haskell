{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
module Tables
  ( ProbData()
  , ProbElement()
  , ProbTable()
  , buildData
  , buildTable
  , tableToList
  )
where

import           Data.Data
import           Data.List
import qualified GHC.Generics                  as G
import           Ranges
import           Text.PrettyPrint.Tabulate     as T

data ProbData = ProbData { n      :: Integer
                         , k      :: Integer
                         , i      :: Double
                         , values :: [Double]
                         } deriving Show

data ProbElement = ProbElement { j                   :: Integer
                                , range              :: String
                                , absoluteFrequency  :: Integer
                                , absoluteFrequency' :: Integer
                                , relativeFrequency  :: Double
                                , relativeFrequency' :: Double
                                , classCenter        :: Double
                                } deriving (Show, Data, G.Generic)

instance T.Tabulate ProbElement T.ExpandWhenNested

data ProbTable = ProbTable { j                  :: [Integer]
                           , range              :: [String]
                           , absoluteFrequency  :: [Integer]
                           , absoluteFrequency' :: [Integer]
                           , relativeFrequency  :: [Double]
                           , relativeFrequency' :: [Double]
                           , classCenter        :: [Double]
                           } deriving (Show, Data, G.Generic)



buildData :: [Double] -> ProbData
buildData vals = ProbData n' k' roundedI sortedValues
  where
    sortedValues = sort vals
    n'            = genericLength vals
    k'            = round $ 1 + 3.32 * (logBase @Double 10 . fromIntegral $ n')
    i'            = (/ fromIntegral k') $ last sortedValues - head sortedValues
    roundedI     = roundTo i' 1


buildTable :: ProbData -> ProbTable
buildTable pdata = ProbTable j' r af af' rf rf' cc
  where
    j'           = [1 .. k pdata]
    tableRanges = createRange (values pdata) (i pdata) (k pdata)
    r           = rangeString tableRanges
    af =
      map (\ran -> genericLength $ values pdata `inRange` ran) tableRanges
    af' = scanl1 (+) af
    rf  = map ((/ (fromIntegral $ n pdata)) . fromIntegral) af  --Thanks hlint
    rf' = scanl1 (+) rf
    cc  = map (uncurry (\f s -> (f + s) / 2)) tableRanges


-- I only discovered too late I needed this, so this is my solution to not rewrite
tableToList :: ProbTable -> [ProbElement]
tableToList (ProbTable [] _ _ _ _ _ _) = []
tableToList (ProbTable (jx : jxs) (r : rs) (af : afs) (af' : afs') (rf : rfs) (rf' : rfs') (cc : ccs))
  = ProbElement jx r af af' rf rf' cc
    : tableToList (ProbTable jxs rs afs afs' rfs rfs' ccs)