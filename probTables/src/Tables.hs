{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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


rawData :: [Double]
rawData =
  [ 3.11
  , 8.88
  , 9.26
  , 10.81
  , 12.69
  , 13.78
  , 15.23
  , 15.62
  , 17.00
  , 17.39
  , 18.36
  , 18.43
  , 19.27
  , 19.50
  , 19.54
  , 20.16
  , 20.59
  , 22.22
  , 23.04
  , 24.47
  , 24.58
  , 25.13
  , 26.24
  , 26.26
  , 27.65
  , 28.06
  , 28.08
  , 28.38
  , 32.03
  , 36.37
  , 38.98
  , 38.64
  , 39.16
  , 41.02
  , 42.97
  , 44.08
  , 44.67
  , 45.40
  , 46.69
  , 48.65
  , 50.39
  , 52.75
  , 54.80
  , 59.07
  , 61.22
  , 70.32
  , 82.70
  , 85.76
  , 86.37
  , 93.34
  ]

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
buildData values = ProbData n k roundedI sortedValues
  where
    sortedValues = sort values
    n            = genericLength values
    k            = round $ 1 + 3.32 * (logBase 10 . fromIntegral $ n)
    i            = (/ fromIntegral k) $ last sortedValues - head sortedValues
    roundedI     = roundTo i 1


buildTable :: ProbData -> ProbTable
buildTable pdata = ProbTable j r af af' rf rf' cc
  where
    j           = [1 .. k pdata]
    firstValue  = head . values $ pdata
    tableRanges = createRange (values pdata) (i pdata) (k pdata)
    r           = rangeString tableRanges
    af =
      map (\range -> genericLength $ values pdata `inRange` range) tableRanges
    af' = scanl1 (+) af
    rf  = map ((/ (fromIntegral $ n pdata)) . fromIntegral) af  --Thanks hlint
    rf' = scanl1 (+) rf
    cc  = map (uncurry (\f s -> (f + s) / 2)) tableRanges


-- I only discovered too late I needed this, so this is my solution to not rewrite
tableToList :: ProbTable -> [ProbElement]
tableToList (ProbTable [] _ _ _ _ _ _) = []
tableToList (ProbTable (j : js) (r : rs) (af : afs) (af' : afs') (rf : rfs) (rf' : rfs') (cc : ccs))
  = ProbElement j r af af' rf rf' cc
    : tableToList (ProbTable js rs afs afs' rfs rfs' ccs)

fullTable :: [ProbElement]
fullTable = tableToList . buildTable . buildData $ rawData
