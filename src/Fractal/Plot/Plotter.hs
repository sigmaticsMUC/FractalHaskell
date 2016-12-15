module Fractal.Plot.Plotter(
  imageFractal
)where

import Codec.Picture



imageFractal :: Int -> [Int] -> String -> IO ()
imageFractal len field path = writePng path $ generateImage pixelRenderer len len
   where pixelRenderer x y = colorList (field !! (len * y + x))



colorList :: Int -> PixelRGB8
colorList count = PixelRGB8 (fromIntegral (count * count * count)) (fromIntegral (count * count * count)) 128
