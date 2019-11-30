module Visualization.Frame
  ( mkFrame ) where

import Data.Array.Accelerate.Data.Colour.RGB

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

mkFrame :: Float -- stroke
         -> Float -- width
         -> Float -- height
         -> Picture
mkFrame s w h
  = let hw = w/2 - s/2
        hh = h/2 - s/2
    in color black
     $ pictures [ translate (-hw) 0     $ rectangleSolid s h
                , translate   hw  0     $ rectangleSolid s h
                , translate 0     hh    $ rectangleSolid w s
                , translate 0     (-hh) $ rectangleSolid w s 
                ]  
