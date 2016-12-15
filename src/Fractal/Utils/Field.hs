module Fractal.Utils.Field(
  generateField,
  generateRow
)where

import Data.Complex

type Value = Complex Double
type Point = Value
type Row = [Point]
type Field = [Row]
type Step = Double


generateField :: Point -> Point -> Step -> Field
generateField start end step
  | (imagPart start) <= (imagPart end)     = (generateRow start end step):[]
  | otherwise                              = (generateRow start end step) : (generateField newStart end step)
      where newStart = (realPart start) :+ ((imagPart start) - step)

generateRow :: Point -> Point -> Step -> Row
generateRow start end step
  | (realPart start) >= (realPart end)  = [start]
  | otherwise                           = start : (generateRow newStart end step)
      where newStart = ((realPart start) + step) :+ (imagPart start)
