module Fractal.Utils.Field(

)where


class Increment a where
  (.+) :: a -> a -> a

generateField :: Increment a => a -> (a, a) -> (a, a) -> [[a]]
generateField start@(xs, ys) end@(xe, ye) = undefined

generateRow :: Increment a => a -> (a, a) -> (a, a) -> [a]
generateRow start@(xs, ys) end@(xe, ye) = undefined
