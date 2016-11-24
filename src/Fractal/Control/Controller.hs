module Fractal.Control.Controller(
  consoleApplication,
  uiApplication
)where

import  System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import qualified Fractal.Parallel.Evaluator as P
import qualified Fractal.Utils.Field as F



type Point = (Double, Double)


consoleApplication :: (a->b) -> IO ()
consoleApplication f = do
  (limit, stepsize, startP, endP, evalStrat') <- consoleReadInput
  let evalStrat = P.stringToStrat evalStrat'
  let field = F.generateField startP endP
  return ()

consoleReadInput :: IO (Double, Double, Point, Point, String)
consoleReadInput = do
  putStr "Type in recursive limit: "
  fractalLimit <- getLine
  let limit = (read fractalLimit) :: Double
  putStr "Type in stepsize: "
  stepSize <- getLine
  let step = (read stepSize) :: Double
  putStr "Type in starting point: "
  startPoint <- getLine
  let startP = (read startPoint) :: Point
  putStr "Type in end point: "
  endPoint <- getLine
  let endP = (read endPoint) :: Point
  putStr "Type in evaluation strategie: "
  evalStrat <- getLine
  return (limit, step, startP, endP, evalStrat)



uiApplication :: IO ()
uiApplication = undefined
