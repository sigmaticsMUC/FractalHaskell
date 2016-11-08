module Fractal.Parallel.Evaluator(
  mandelEval,
  EvalStrat (Single, Parallel, Threaded),
)where

import Control.Parallel.Strategies

data EvalStrat = Single | Parallel | Threaded


mandelEval :: Num a => EvalStrat -> (a -> a) -> [[a]] -> [[a]]
mandelEval strat f domain = case strat of
  Single -> mandelSingle f domain
  Parallel -> mandelParallel f domain
  Threaded -> mandelThreaded f domain


{-

 Top Level Helping Functions

-}

mandelSingle :: Num a => (a->a) -> [[a]] -> [[a]]
mandelSingle f domain = map (\row -> map f row) domain

mandelParallel :: Num a => (a->a) -> [[a]] -> [[a]]
mandelParallel f domain = runEval $ do
  resultDomain <- rseq $ domainParralel f domain
  return resultDomain

mandelThreaded :: Num a => (a->a) -> [[a]] -> [[a]]
mandelThreaded f domain = undefined


{-

Low Level Helping Functions

-}


domainParralel :: Num a => (a->a) -> [[a]] -> [[a]]
domainParralel _ [] = []
domainParralel f (row:rows) = (rowParralel f row) : (domainParralel f rows)

rowParralel :: Num a => (a->a) -> [a] -> [a]
rowParralel f row = runEval $ do
  rowResult <- rpar $ map f row
  return rowResult
