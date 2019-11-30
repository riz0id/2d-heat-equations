{-# LANGUAGE ViewPatterns #-} 

module Visualization.Field
  ( renderField ) where

import qualified Prelude as P
import qualified Data.List as L
import qualified Debug.Trace as Debug

import Data.Array.Accelerate                  as A
import Data.Array.Accelerate.Data.Colour.HSV  as C
import Data.Array.Accelerate.Data.Colour.RGB  as C
import Data.Array.Accelerate.Data.Colour.RGBA as C
import Data.Array.Accelerate.LLVM.Native      as CPU

import Graphics.Gloss
import Graphics.Gloss.Accelerate.Data.Picture

import Type
import Visualization.Colour 

maxFrom :: Acc (Field Entropy) -> Exp Entropy
maxFrom hF = the . maximum . flatten $ hF

minFrom :: Acc (Field Entropy) -> Exp Entropy
minFrom hF = the . minimum . flatten $ hF

renderField :: Field Entropy -> ColourSch -> Picture
renderField (use -> hF) csch =
  let maxT = maxFrom hF
      minT = minFrom hF
      cF   = CPU.run 
           . map (\t -> packRGB
                     . toRGB
                     $ interpolate csch ((t-minT)/(maxT-minT)))
           $ hF
  in bitmapOfArray cF True
                                      
