
module Main where

import qualified Fractal.Control.Controller as C
import  System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Environment



main :: IO ()
main = do
  arg <- getArgs
  C.mandelApplication (C.getApplicationType arg)
  return ()
