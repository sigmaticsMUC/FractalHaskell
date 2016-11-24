module Fractal.Gui.FractalWindow(
  main
)where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import System.Glib.Signals (on)
import Control.Monad.IO.Class
import Graphics.UI.Gtk.Buttons.Button
import Graphics.UI.Gtk.Entry.Entry
import qualified Fractal.Gui.UI_ID as GUI_ID




data MandelUI = MandelUI{
                  _window          :: Window
                  --calcButton      :: Button,
                  --limitEntry      :: Entry,
                  --stepEntry       :: Entry,
                  --x_startEntry    :: Entry,
                  --y_startEntry    :: Entry,
                  --x_endEntry      :: Entry,
                  --y_endEntry      :: Entry,
                  --evalStratEntry  :: Entry
                }

createUI :: Builder -> IO MandelUI
createUI builder = do
  let builderAcces = builderGetObject builder
  win     <- builderAcces castToWindow GUI_ID.id_WINDOW
  clcB    <- builderGetObject builder castToButton GUI_ID.id_CALC_BUTTON
  lim     <- builderGetObject builder castToEntry GUI_ID.id_LIMIT
  step    <- builderGetObject builder castToEntry GUI_ID.id_STEP_SIZE
  x_s     <- builderGetObject builder castToEntry GUI_ID.id_X_START
  y_s     <- builderGetObject builder castToEntry GUI_ID.id_Y_END
  x_e     <- builderGetObject builder castToEntry GUI_ID.id_X_END
  y_e     <- builderGetObject builder castToEntry GUI_ID.id_Y_END
  calcT   <- builderGetObject builder castToEntry GUI_ID.id_EVAL_STRAT
  --return (MandelUI { window=win, calcButton=clcB, limitEntry=lim, stepEntry=step, x_startEntry=x_s, y_startEntry=y_s, x_endEntry=x_e, y_endEntry=y_e, evalStratEntry=calcT})
  return MandelUI {_window = win}

entryAction entry = do
  res <- entryGetText entry
  putStrLn res
  return res


main = do
    initGUI
    hello    <- builderNew
    builderAddFromFile hello "./res/interface3.glade"
    mandelUI <- createUI hello
    --(on) window deleteEvent $ liftIO mainQuit >> return False
    --(on) clcButton buttonActivated $ putStrLn "HI"
    widgetShowAll (_window mandelUI)
    mainGUI
