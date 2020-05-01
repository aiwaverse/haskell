module Main
    ( main
    )
where

import           Lesson24
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    let files = (,) (fromJust firstElement) (fromJust secondElement)
          where
            firstElement  = viaNonEmpty head args
            secondElement = viaNonEmpty last . take 2 $ args
            fromJust      = fromMaybe ""
    uncurry cp files
