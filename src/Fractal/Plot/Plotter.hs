module Fractal.Plot.Plotter(

)where
{-
 Nur ein vorschlag, eigene class braucht es nicht!
-}
type RGB = (Float, Float, Float)

class Plottable a where
  plot :: [[a]] -> [[RGB]]
