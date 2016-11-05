module Fractal.Parallel.Evaluator(
  mandelEval,
  EvalStrat (Single, Parallel, Threaded)
)where


data EvalStrat = Single | Parallel | Threaded


mandelEval :: Num a => EvalStrat -> (a -> a) -> [[a]] -> [[a]]
mandelEval strat f domain = case strat of
  Single -> mandelSingle f domain
  Parallel -> mandelParallel f domain
  Threaded -> mandelThreaded f domain

mandelSingle :: Num a => (a->a) -> [[a]] -> [[a]]
mandelSingle f domain = map (\row -> map f row) domain

mandelParallel :: Num a => (a->a) -> [[a]] -> [[a]]
mandelParallel f domain = undefined

mandelThreaded :: Num a => (a->a) -> [[a]] -> [[a]]
mandelThreaded f domain = undefined
