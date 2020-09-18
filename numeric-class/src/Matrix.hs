module Matrix() where

import           Data.Matrix hiding
    (fromList)
import qualified Data.Matrix as Mtr
    (fromList)

class Range a where
  (<!>) :: Matrix b -> a -> Matrix b

instance Range Int where
  m <!> (line, colo)

getRange :: Matrix a -> ((Int, Int),(Int, Int)) -> Matrix a
getRange m ((startLine, endLine),(startCol, endCol)) = undefined
