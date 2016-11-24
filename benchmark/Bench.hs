module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, nf)
import qualified Fractal.Parallel.Evaluator as P


test_f :: Int -> Int
test_f x = x ^ 8

test_array :: [[Int]]
test_array = [ [1..1000] | _ <- [1..1000]]


square :: Num a => a -> a
square x = x * x

singleEvaluation = P.mandelEval P.Single test_f
parralelEvaluation = P.mandelEval P.Parallel test_f
repaSingleEvaluation = P.mandelEval P.RepaSingle test_f
repaParallelEvaluation = P.mandelEval P.RepaParallel test _f

main :: IO ()
main = defaultMain
  [ bgroup "Evauluator" [ bench "Single"    $ nf singleEvaluation test_array
                        , bench "Parallel"  $ nf parralelEvaluation test_array
                        , bench "Single Repa" $ nf repaSingleEvaluation test_array
                        , bench "Parallel Repa" $ repaParallelEvaluation test_array
                        ]
  ]
