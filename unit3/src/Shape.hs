module Shape where

-- circle has one measure
newtype Circle = Circle { radius :: Double }

-- square has just one measure, side
newtype Square = Square { side :: Double }

-- rectangle has two measurements, height and widht
data Rectangle =
  Rectangle { height :: Double
            , width  :: Double
            }

data Shape
  = ShapeCirc Circle
  | ShapeSqr Square
  | ShapeRect Rectangle

perimeter :: Shape -> Double
perimeter (ShapeCirc c) = 2 * 3.14 * circleRadius
  where circleRadius = radius c

perimeter (ShapeSqr  s) = (* 4) . side $ s

perimeter (ShapeRect r) = 2 * rectWidth + 2 * rectHeight
 where
  rectWidth  = width r
  rectHeight = height r

area :: Shape -> Double
area (ShapeCirc c) = ((* 3.14) . radius $ c) ^ 2

area (ShapeSqr  s) = (^ 2) . side $ s

area (ShapeRect r) = rectWidth * rectHeight
 where
  rectWidth  = width r
  rectHeight = height r


