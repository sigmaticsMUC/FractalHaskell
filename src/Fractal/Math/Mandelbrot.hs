module Fractal.Math.Mandelbrot(
  mandel,
  mandel'
)where
{-

Math packet kommt alles mathe bezogene rein

-}

import Data.Complex
import Fractal.Math.Kernel (evaluateFractal)

type Point = Complex Double
type Limit = Double
type Iteration = Complex Double
type Magnitude = Double
type MaxIter = Int


mandelEquation :: Point -> Iteration -> Iteration
mandelEquation c (0.0 :+ 0.0) = c
mandelEquation c (0.0 :+ 1) = 0
mandelEquation c (0.0 :+ (-1)) = 0
mandelEquation c z = z * z + c

mandel :: MaxIter -> Limit -> [Point] -> [Int]
mandel maxIter l field = map mandelEvaluator field
  where mandelEvaluator = evaluateFractal mandelEquation l maxIter

mandel' :: MaxIter -> Limit -> Point -> Int
mandel' maxIter l field = mandelEvaluator field
  where mandelEvaluator = evaluateFractal mandelEquation l maxIter
