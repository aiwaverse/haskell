module Lesson23
    ( helloWorld
    , sumNumbers
    )
where

import           Data.Maybe
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.IO             as LTIO

helloPerson :: Text -> Text
helloPerson name = "Hello" <> " " <> name <> "!"

helloWorld :: IO ()
helloWorld = do
    putTextLn "Hello! What's your name?"
    name <- getLine
    let statement = helloPerson name
    putTextLn statement

toInts :: LT.Text -> [Int]
toInts = fmap (fromJust . readMaybe . toString) . LT.lines

sumNumbers :: IO ()
sumNumbers = do
    putTextLn "Enter the numbers:"
    userInput <- LTIO.getContents
    let numbers = toInts userInput
    putText "Sum: "
    print (sum numbers)
