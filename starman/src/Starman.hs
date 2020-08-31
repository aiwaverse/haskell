{- |
Copyright: (c) 2020 Aiwa
SPDX-License-Identifier: MIT
Maintainer: Aiwa <aiwavision@protonmail.com>

See README for more info
-}

module Starman
    ( check
    , turn
    , makeGuess
    , starman
    )
where

import qualified Data.Text as T(length)

check :: Text -> Text -> Maybe Char -> (Bool, Text)
check word display (Just guess) = (,) (guess `elem` toString word) $ toText
    [ if x == guess then x else y | (x, y) <- zip (toString word) (toString display) ]

check _ display Nothing         = (False, display)

turn :: Text -> Text -> Int -> IO ()
turn word display n | n == 0          = putTextLn "You lose"
                    | word == display = putTextLn "You win!"
                    | otherwise       = makeGuess word display n

makeGuess :: Text -> Text -> Int -> IO ()
makeGuess word display n = do
    putTextLn $ display <> " " <> toText (replicate n '*')
    putTextLn "Enter your guess > "
    guess <- getLine
    let firstLetter = viaNonEmpty head $ toString guess
    when (isNothing firstLetter) $ turn word display n
    let (correct, display') = check word display firstLetter
    let n' | correct   = n
           | otherwise = n - 1
    turn word display' n'

starman :: Text -> Int -> IO ()
starman word = makeGuess word (toText $ replicate chars '-')
  where chars = T.length word
