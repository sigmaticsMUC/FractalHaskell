{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Fractal.Parallel.Evaluator(
  mandelEval,
  EvalStrat (Single, Parallel, RepaSingle, RepaParallel),
  stringToStrat,
)where


import Control.Parallel.Strategies
import Control.DeepSeq
import qualified Data.Array.Repa as R
import Control.Monad.Identity
import Data.Char

data EvalStrat = Single | Parallel | RepaSingle | RepaParallel
  deriving (Show, Eq)

type RepaFractal r = R.Array R.U R.DIM1 r


mandelEval strat f domain = case strat of
  Single -> mandelSingle f domain
  Parallel -> mandelParallel f domain
  RepaSingle -> R.toList (applySingleRepa f domain :: R.Array R.U R.DIM1 Int)
  RepaParallel -> R.toList (applyParallelRepa f domain :: R.Array R.U R.DIM1 Int)


{-

 Top Level Helping Functions

-}

mandelSingle f domain = map force (map f domain)

mandelParallel f domain = domainParralel f domain

applySingleRepa f domain = applySingleRepa' f (convertList domain)

applyParallelRepa f domain = applyParallelRepa' f (convertList domain)

stringToStrat :: String -> EvalStrat
stringToStrat typeIn = case typeIn' of
  "PARALLEL"     -> Parallel
  "REPASINGLE"   -> RepaSingle
  "REPAPARALLEL" -> RepaParallel
  _              -> Single
  where typeIn' = map toUpper typeIn


{-

Low Level Helping Functions

-}

domainParralel :: (Num b, Num a, NFData b, NFData a) => (a->b) -> [a] -> [b]
domainParralel f domain = parallelComputedList
  where mapped = map f domain
        parallelComputedList = mapped `using` parList rdeepseq


convertList list = R.fromListUnboxed (R.Z R.:. rows) list
  where rows = length list


applySingleRepa' f domain     = R.computeS (R.fromFunction (R.Z R.:.n) sp)
  where R.Z R.:. n      = R.extent domain
        sp (R.Z R.:.i ) =  f (domain R.! (R.Z R.:.i))

applyParallelRepa' f domain     = runIdentity $  res
  where R.Z R.:.n     = R.extent domain
        sp (R.Z R.:.i ) =  f (domain R.! (R.Z R.:.i))
        res = do
          res' <- R.computeP (R.fromFunction (R.Z R.:.n) sp)
          return res'
