{-# LANGUAGE FlexibleContexts #-}

module Fractal.Parallel.Evaluator(
  mandelEval,
  EvalStrat (Single, Parallel, Repa),
  applySingleRepa
)where


import Control.Parallel.Strategies
import Control.DeepSeq
import qualified Data.Array.Repa as R
import Control.Monad.Identity

data EvalStrat = Single | Parallel | RepaSingle | RepaParallel

type RepaFractal r = R.Array R.U R.DIM2 r


mandelEval :: (Num a, NFData a) => EvalStrat -> (a -> a) -> [[a]] -> [[a]]
mandelEval strat f domain = case strat of
  Single -> mandelSingle f domain
  Parallel -> mandelParallel f domain
  Repa -> mandelRepa f domain


{-

 Top Level Helping Functions

-}

mandelSingle :: Num a => (a->a) -> [[a]] -> [[a]]
mandelSingle f domain = map (\row -> map f row) domain

mandelParallel :: (Num a, NFData a) => (a->a) -> [[a]] -> [[a]]
mandelParallel f domain = runEval $ do
  resultDomain <- rseq $ domainParralel f domain
  return resultDomain

mandelRepa :: Num a => (a->a) -> [[a]] -> [[a]]
mandelRepa f domain = undefined


{-

Low Level Helping Functions

-}

-- Apply function to 2D Array.
domainParralel :: (Num a, NFData a) => (a->a) -> [[a]] -> [[a]]
domainParralel _ [] = []
domainParralel f (row:rows) = (rowParralel f row) : (domainParralel f rows)

-- Apply function to List within rpar.
rowParralel :: (Num a, NFData a) => (a->a) -> [a] -> [a]
rowParralel f row = runEval $ do
  rowResult <- (rpar `dot` rdeepseq) $ map f row
  return rowResult

--convertList :: Num a => [[a]] -> R.Array R.U R.DIM2 a
convertList list@(row:_) = R.fromListUnboxed (R.Z R.:. rows R.:. cols) flated
  where rows = length list
        cols = length row
        flated = concat list

applySingleRepa f domain = applySingleRepa' f (convertList domain)

applyParallelRepa f domain = applyParallelRepa' f (convertList domain)

--applySingleRepa :: Num a => (a->a) -> RepaFractal a -> RepaFractal a
applySingleRepa' f domain     = R.computeS (R.fromFunction (R.Z R.:.n R.:.n) sp)
  where R.Z R.:._ R.:.n      = R.extent domain
        sp (R.Z R.:.i R.:.j) =  f (domain R.! (R.Z R.:.i R.:.j))

applyParallelRepa' f domain     = runIdentity $  res
  where R.Z R.:._ R.:.n      = R.extent domain
        sp (R.Z R.:.i R.:.j) =  f (domain R.! (R.Z R.:.i R.:.j))
        res = do
          res' <- R.computeP (R.fromFunction (R.Z R.:.n R.:.n) sp)
          return res'
