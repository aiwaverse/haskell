module Lesson23
    ( helloWorld
    , sumNumbers
    )
where

import           Data.Maybe
import qualified Data.Text.Lazy                as Lazy
import qualified Data.Text.Lazy.IO             as LazyIO
                                                ( getContents )

helloPerson :: Text -> Text
helloPerson name = "Hello" <> " " <> name <> "!"

helloWorld :: IO ()
helloWorld = do
    putTextLn "Hello! What's your name?"
    name <- getLine
    let statement = helloPerson name
    putTextLn statement

toInts :: Lazy.Text -> [Int]
toInts = map fromJust . fmap readMaybe . map Lazy.unpack . Lazy.lines

sumNumbers :: IO ()
sumNumbers = do
    putTextLn "Enter numbers:"
    userInput <- LazyIO.getContents
    let numbers = toInts userInput
    putText "Sum: "
    print (sum numbers)
