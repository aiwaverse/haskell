cup :: Integer -> (Integer -> t2) -> t2
-- will wait for a function to operate in ml
cup ml message = message ml

getMl :: ((p -> p) -> t) -> t
-- since ml is the only argument, id ml returns ml
getMl aCup = aCup id

drink :: ((p -> p) -> Integer) -> Integer -> (Integer -> t2) -> t2
drink aCup mlDrank | mlDifference > 0 = cup mlDifference
                   | otherwise = cup 0
    where currentMl = getMl aCup
          mlDifference = currentMl - mlDrank