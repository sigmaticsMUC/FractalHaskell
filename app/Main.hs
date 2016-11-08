module Main where

import qualified Fractal.Parallel.Evaluator as P
import  System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = putStrLn "HI"


test_f :: Int -> Int
test_f x = x ^ 8

test_array :: [[Int]]
test_array = [ [1..1000] | _ <- [1..1000]]
