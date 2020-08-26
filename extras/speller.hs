-- | This function takes a list of strings @xs@, and returns a string with the
-- speller book style
-- > speller ["apple", "banana"] === "a is for apple, and b is for banana"
speller :: [String] -> String
speller = makeReadablePhrase . map oneWordSpeller

-- | This function creates a basic speller book phrase for one word
-- > oneWordSpeller "car" === "c is for car"
oneWordSpeller :: String -> String
oneWordSpeller []          = []
oneWordSpeller lst@(x : _) = x : " is for " <> lst -- pattern matching with the @ to make it easier (and avoid head)

-- | This function takes a list of strings @xs@ already on the speller book format and
-- concatenate them in a way it matches english wording
-- > makeReadablePhrase ["a is for apple", "b is for banana"] === "a is for apple, and b is for banana"
makeReadablePhrase :: [String] -> String
makeReadablePhrase []       = []
makeReadablePhrase [x]      = x
makeReadablePhrase [x, xs]  = x <> ", and " <> xs -- (<>) is the monoid equivalent of mconcat, habit of using Text, same as (++)
makeReadablePhrase (x : xs) = x <> ", " <> makeReadablePhrase xs
