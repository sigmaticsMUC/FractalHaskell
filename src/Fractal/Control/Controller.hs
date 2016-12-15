module Fractal.Control.Controller(
  ApplicationType (UI_Application, Console_Application),
  mandelApplication,
  getApplicationType
)where

import  System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import qualified Fractal.Parallel.Evaluator as P
import qualified Fractal.Gui.FractalWindow as W
import qualified Fractal.Utils.Field as F
import Fractal.Math.Mandelbrot (mandel, mandel')
import Data.Complex
import Fractal.Plot.Plotter (imageFractal)
import Data.DateTime


data ApplicationType = UI_Application | Console_Application | None
type Point = Complex Double

mandelApplication :: ApplicationType -> IO ()
mandelApplication appType = case appType of
  UI_Application        -> uiApplication
  Console_Application   -> consoleApplication
  None                  -> error "Blah"

getApplicationType :: [String] -> ApplicationType
getApplicationType [] = None
getApplicationType (x:_) = case x of
  "UI_Application"      -> UI_Application
  "Console_Application" -> Console_Application
  _                     -> None


consoleApplication :: IO ()
consoleApplication = do
  input <- consoleReadInput
  coreComputation input
  return ()

coreComputation :: (Double, Int, Double, Point, Point, String) -> IO ()
coreComputation (limit, maxIter, stepSize, sPoint, ePoint, strat) = do
  let field   = F.generateField sPoint ePoint stepSize
  let field'  = concat field
  let res     = P.mandelEval (P.stringToStrat strat) (mandel' maxIter limit) field'
  let len     = length $ head field
  dateTime    <- getCurrentTime
  let (year, month, day) = toGregorian' dateTime
  imageFractal len res ("./Mandel_" ++ (show year) ++ "_" ++ (show month) ++ "_" ++ (show day) ++ "_" ++ (show $ toSeconds dateTime))
  return ()


uiApplication :: IO ()
uiApplication = W.mainW coreComputation

{-
  Helper functions
-}
consoleReadInput :: IO (Double, Int, Double, Point, Point, String)
consoleReadInput = do
  putStr "Type in recursive limit: "
  fractalLimit <- getLine
  let limit = (read fractalLimit) :: Double
  putStr "Type in max number of iterations: "
  maxIter <- getLine
  let maxIt = (read maxIter) :: Int
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
  return (limit, maxIt, step, startP, endP, evalStrat)
