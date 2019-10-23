type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi discs from to aux =
    hanoi (discs - 1) from aux to ++ 
    [(from, to)] ++ 
    hanoi (discs - 1) aux to from
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n from to aux1 aux2 = undefined