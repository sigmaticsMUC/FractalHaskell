module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, nf)
import Fractal.Parallel.Evaluator (mandelEval,   EvalStrat (Single, Parallel, RepaSingle, RepaParallel))
import qualified Data.Array.Repa as R


test_f :: Int -> Int
test_f x = x ^ 2

test_array :: [Int]
test_array = [1..10000000]


square :: Num a => a -> a
square x = x * x

singleEvaluation = mandelEval Single test_f
parralelEvaluation = mandelEval Parallel test_f
--res :: R.Array R.U R.DIM2 Int
--res = P.applySingleRepa test_f test_array
repaSingleEvaluation :: [Int] -> [Int]
repaSingleEvaluation = mandelEval RepaSingle test_f
--repaParallelEvaluation = P.mandelEval P.RepaParallel test_f

main :: IO ()
main = defaultMain
  [ bgroup "Evauluator" [ bench "Single"    $ nf singleEvaluation test_array
                        , bench "Parallel"  $ nf parralelEvaluation test_array
                        --, bench "Single Repa" $ nf repaSingleEvaluation test_array
                        --, bench "Parallel Repa" $ nf repaParallelEvaluation test_array
                        ]
  ]
