toDigits :: Integer -> [Integer]
toDigits num = 
    reverse (toDigitsRev num)

toDigitsRev :: Integer -> [Integer]
toDigitsRev num
    | num <= 0  = []
    | otherwise = (num `mod` 10) : toDigitsRev (num `div` 10)

    
{- makes an indexed list out an infnite list and the reverse list, decides if double or
    not, then re-reverse the list-}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse [doubleOrNot x i | (i,x) <- zip [0..] rl]
    where rl = reverse l

doubleOrNot :: Integer -> Integer -> Integer
doubleOrNot n i = if odd i then n*2 else n

-- combining results
sumDigits :: [Integer] -> Integer
sumDigits list = sum (correctDigits list)

-- used eta reduction, this map takes the list as argument too 
correctDigits :: [Integer] -> [Integer]
correctDigits = map sumBreak

-- sum digits of a number with n digits
sumBreak :: Integer -> Integer
sumBreak n
    | n < 10 = n
    | otherwise = sum (toDigits n)

validate :: Integer -> Bool
validate card
    | sumDigits doubleCardDigits `mod` 10 == 0 = True
    | otherwise                                = False
    where cardDigits = toDigits card
          doubleCardDigits = doubleEveryOther cardDigits