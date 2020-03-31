data Clock = Clock Int Int deriving Eq

instance Show Clock where
  show (Clock h m) = correctedH ++ ":" ++ correctedM
    where
      correctedH = if h > 9 then show h else '0' : show h
      correctedM = if m > 9 then show m else '0' : show m

type HourMin = (Int, Int)

minuteTaker :: Int -> HourMin
minuteTaker m = if m > 0 then minuteTakerP else minuteTakerN
  where
    minuteTakerP = (,) (m `div` 60) (m `mod` 60)
    minuteTakerN = (,) (negate $ abs m `div` 60) (negate $ abs m `mod` 60)

addHourToClock :: Clock -> HourMin -> Clock
addHourToClock (Clock hour minute) hm =
  adjustClock $ Clock (hour + fst hm) (minute + snd hm)

adjustClock :: Clock -> Clock
adjustClock (Clock h m) = Clock hour mins
  where
    (addH, addM) = minuteTaker m
    hour         = (h + addH + addM `div` 60) `mod` 24
    mins         = addM + (60 * abs (addM `div` 60))
