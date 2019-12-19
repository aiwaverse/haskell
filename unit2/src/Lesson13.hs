module Lesson13 where

data SixSidedDie
   = S1
   | S2
   | S3
   | S4
   | S5
   | S6

instance Show SixSidedDie where
   show S1 = "one"
   show S2 = "two"
   show S3 = "three"
   show S4 = "four"
   show S5 = "five"
   show S6 = "six"

instance Eq SixSidedDie where
   (==) S1 S1 = True
   (==) S2 S2 = True
   (==) S3 S3 = True
   (==) S4 S4 = True
   (==) S5 S5 = True
   (==) S6 S6 = True
   (==) _ _ = False

