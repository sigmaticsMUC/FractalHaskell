module Fractal.Math.Kernel(
  evaluateFractal
)where

import Data.Complex

type Point = Complex Double
type Limit = Double
type Iteration = Complex Double
type Magnitude = Double
type MaxIter = Int
type Iter = Int


evaluateFractal :: (Point -> Iteration -> Iteration) -> Limit -> MaxIter -> Point -> Iter
evaluateFractal f l m c = res c (0.0 :+ 0.0) 0
  where res c z i             = if i == m then i else rest (f c z) i
        rest val i             = if checkAcceptance l (magnitude val) then i else res c val (i + 1)


checkAcceptance :: Limit -> Double -> Bool
checkAcceptance l val
  | val >= l    = True
  | val == 0.0  = True
  | otherwise   = False
