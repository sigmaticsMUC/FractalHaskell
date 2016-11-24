module Fractal.Utils.Field(
  generateField
)where
import Data.Complex
type FieldPoint = (Double, Double)
type ComplexPoint = (Complex Double, Complex Double)
type Field = [[Complex Double]]

{-
class Increment a where
  (.+) :: a -> a -> a

generateField :: Increment a => a -> (a, a) -> (a, a) -> [[a]]
generateField start@(xs, ys) end@(xe, ye) = undefined

generateRow :: Increment a => a -> (a, a) -> (a, a) -> [a]
generateRow start@(xs, ys) end@(xe, ye) = undefined
-}

generateField :: FieldPoint -> FieldPoint -> Field
generateField start end = undefined
