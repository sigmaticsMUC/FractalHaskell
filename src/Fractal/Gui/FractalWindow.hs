module Fractal.Gui.FractalWindow(
  mainW
)where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import System.Glib.Signals (on)
import Control.Monad.IO.Class
import Graphics.UI.Gtk.Buttons.Button
import Graphics.UI.Gtk.Entry.Entry
import qualified Fractal.Gui.UI_ID as GUI_ID
import Graphics.UI.Gtk.Display.Image
import Graphics.UI.Gtk.MenuComboToolbar.ComboBox
import Control.Monad
import Data.Complex
import Data.Maybe
import Graphics.UI.Gtk.Display.Spinner
import Control.Concurrent

data MandelUI = MandelUI{
                  _window          :: Window,
                  _calcButton      :: Button,
                  _limitEntry      :: Entry,
                  _iterEntry       :: Entry,
                  _stepEntry       :: Entry,
                  _x_startEntry    :: Entry,
                  _y_startEntry    :: Entry,
                  _x_endEntry      :: Entry,
                  _y_endEntry      :: Entry,
                  _evalStratEntry  :: ComboBox,
                  _spinner         :: Spinner
                  --_image           :: Image
                }


type MandelInput = (Double, Int, Double, Complex Double, Complex Double, String)

createUI :: Builder -> IO MandelUI
createUI builder = do
  let builderAcces = builderGetObject builder
  win     <- builderAcces castToWindow GUI_ID.id_WINDOW
  clcB    <- builderGetObject builder castToButton GUI_ID.id_CALC_BUTTON
  lim     <- builderGetObject builder castToEntry GUI_ID.id_LIMIT
  step    <- builderGetObject builder castToEntry GUI_ID.id_STEP_SIZE
  x_s     <- builderGetObject builder castToEntry GUI_ID.id_X_START
  y_s     <- builderGetObject builder castToEntry GUI_ID.id_Y_START
  iter    <- builderGetObject builder castToEntry GUI_ID.id_MAX_ITER
  x_e     <- builderGetObject builder castToEntry GUI_ID.id_X_END
  y_e     <- builderGetObject builder castToEntry GUI_ID.id_Y_END
  calcT   <- builderGetObject builder castToComboBox GUI_ID.id_EVAL_STRAT
  spinn   <- builderGetObject builder castToSpinner GUI_ID.id_SPINNER
  --img     <- builderGetObject builder castToImage GUI_ID.id_IMAGE
  return (MandelUI { _window=win, _calcButton=clcB, _limitEntry=lim
                   , _stepEntry=step, _x_startEntry=x_s, _iterEntry=iter, _y_startEntry=y_s
                   , _x_endEntry=x_e, _y_endEntry=y_e, _spinner=spinn, _evalStratEntry=calcT{-, _image=img-}})

buttonClick :: ((Double, Int, Double, Complex Double, Complex Double, String) -> IO ()) -> MandelUI -> IO ()
buttonClick f ui = do
  let entrys =  getEntrys ui
  stringInput <- readEntrys entrys
  evalStrat   <- readEvaluationMode (_evalStratEntry ui)
  let mandelInput = readMandelInput evalStrat stringInput
  putStrLn $ show mandelInput
  spinnerStart (_spinner ui)
  forkFinally (f mandelInput) (\_ -> spinnerStop (_spinner ui))
  return ()

getEntrys :: MandelUI -> [Entry]
getEntrys ui = [_limitEntry ui, _iterEntry ui, _stepEntry ui, _x_startEntry ui, _y_startEntry ui
               , _x_endEntry ui, _y_endEntry ui]


readMandelInput :: String -> [String] -> MandelInput
readMandelInput _ [] = error "No Parameters found!"
readMandelInput eval blah@[lim, iter, step, xS, yS, xE, yE] =
  let lim'  = (read lim) :: Double
      step' = (read step) :: Double
      iter' = (read iter) :: Int
      xS'   = (read xS) :: Double
      yS'   = (read yS) :: Double
      xE'   = (read xE) :: Double
      yE'   = (read yE) :: Double
  in (lim', iter', step', xS' :+ yS', xE':+ yE', eval)

readEvaluationMode :: ComboBox -> IO String
readEvaluationMode box = do
  active <- comboBoxGetActive box
  putStrLn $ show active
  let a = case active of
            1 -> "Parallel"
            0 -> "Single"
            2 -> "RepaSingle"
            _ -> "RepaParallel"
  return a

readEntrys :: [Entry] -> IO [String]
readEntrys entrys = do
  input <- mapM entryGetText entrys
  return input

mainW :: ((Double, Int, Double, Complex Double, Complex Double, String) -> IO ()) -> IO ()
mainW computation = do
    initGUI
    builder    <- builderNew
    builderAddFromFile builder GUI_ID.path_INTERFACE
    mandelUI <- createUI builder
    --imageSetFromFile (_image mandelUI) GUI_ID.path_IMAGE
    (on) (_window mandelUI)    deleteEvent $ liftIO mainQuit >> return False
    (on) (_calcButton mandelUI) buttonActivated $ buttonClick computation mandelUI
    widgetShowAll (_window mandelUI)
    mainGUI
