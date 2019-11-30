{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module Visualization.Graph
  (heatMap) where

import Type

import qualified Prelude as P

import Data.Array.Accelerate                 as A
import Data.Array.Accelerate.Data.Colour.HSV as C
import Data.Array.Accelerate.Data.Colour.RGB as C
import Data.Array.Accelerate.LLVM.Native     as CPU

import Graphics.Gloss
import Graphics.Gloss.Accelerate.Data.Picture

import Visualization.Frame
import Visualization.Field
import Visualization.Colour

type AspectRatio = (Float, Float)

heatMap :: Field Entropy
        -> ColourSch
        -> AspectRatio
        -> Picture
heatMap hF csch (wr, hr) =
  let Z :. h :. w = arrayShape hF
      wFloat      = P.fromIntegral w :: Float
      hFloat      = P.fromIntegral h :: Float
      fieldImg    = pictures [ renderField hF csch
                             , mkFrame 2 wFloat hFloat ]
      graphScale  = translate (wFloat/2+50) 0
                  $ pictures [ colorGrad csch 50 h
                             , mkFrame 2 50 hFloat ]
  in pictures [ fieldImg, graphScale ]
                
colorGrad :: ColourSch -> Int -> Int -> Picture
colorGrad csch (constant -> w) (constant -> h) =
  let go (A.fst . unindex2 -> ix)
        = C.packRGB
        . toRGB
        $ interpolate csch (toFloating ix / toFloating h)
  in bitmapOfArray (CPU.run $ A.generate (index2 h w) go) True
