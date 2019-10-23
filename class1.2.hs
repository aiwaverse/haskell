type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi discs from to aux =
    hanoi (discs - 1) from aux to
        ++ [(from, to)]
        ++ hanoi (discs - 1) aux to from
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 (-1) _ _ _ _ = []
hanoi4 0    _ _ _ _ = []
hanoi4 n from to aux1 aux2 =
    hanoi4 (n - 2) from aux1 aux2 to
        ++ [(from, aux2)]
        ++ [(from, to)]
        ++ [(aux2, to)]
        ++ hanoi4 (n - 2) aux1 to from aux2
