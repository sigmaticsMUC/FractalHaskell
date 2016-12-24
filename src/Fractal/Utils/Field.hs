module Fractal.Utils.Field(
  generateField,
  generateRow,
  generateRowField,
  getFieldLength
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


generateRowField :: Point -> Point -> Step -> Row
generateRowField start end step = generateRowField' start start end step


generateRowField' :: Point -> Point -> Point -> Step -> Row
generateRowField' mark start end step
  | (imagPart start) <= (imagPart end) = endroutine
  | otherwise = fieldroutine
    where endroutine = if (realPart start) >= (realPart end) then [start] else start : (generateRowField' mark newStart end step)
          fieldroutine = if (realPart start) >= (realPart end) then start:(generateRowField' mark newRowStart end step) else start : (generateRowField' mark newStart end step)
          newStart = ((realPart start) + step) :+ (imagPart start)
          newRowStart = (realPart mark) :+ ((imagPart start) - step)

getFieldLength :: Point -> Point -> Step -> Int
getFieldLength a b step = count a b 0
  where count x y i = if (realPart x) >= (realPart y) then (i+1) else count (newX x) y (i+1)
        newX x = ((realPart x) + step) :+ (imagPart x)
