

module Main where

import qualified Data.Array.Repa as R
import qualified Fractal.Parallel.Evaluator as P
import  System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = putStrLn "HI"


test_f :: Int -> Int
test_f x = x ^ 8

test_array :: [[Int]]
test_array = [ [1..100] | _ <- [1..100]]

res :: R.Array R.U R.DIM2 Int
res = P.applySingleRepa test_f test_array
