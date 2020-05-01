module Lesson24
    ( getCounts
    , countsToText
    , countsFile
    )
where

import qualified Data.Text                     as T
import           System.Environment

getCounts :: Text -> (Int, Int, Int)
getCounts t = (,,) (T.length t) (length linesT) (length wordsT)
  where
    linesT = lines t
    wordsT = words t

countsToText :: (Int, Int, Int) -> Text
countsToText (cc, lc, wc) =
    mconcat ["chars: ", show cc, ",lines: ", show lc, ",words: ", show wc]

countsFile :: IO ()
countsFile = do
    args <- getArgs
    let fileName = fromMaybe "" $ viaNonEmpty head args
    fileContens <- readFileText fileName
    let counts = countsToText . getCounts $ fileContens
    appendFileText "stats.dat" $ mconcat [toText fileName, " stats:\n", counts]
    putTextLn counts
