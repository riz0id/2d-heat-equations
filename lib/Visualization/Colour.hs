{-# LANGUAGE FlexibleContexts #-}

module Visualization.Colour
  ( ColourSch
  , redBlue
  , blackWhite
  , mkColourScheme
  , interpolate
  ) where

import qualified Data.Array.Accelerate                 as A
import           Data.Array.Accelerate.Data.Colour.HSV as C
import           Data.Array.Accelerate.LLVM.Native     as CPU

type ColourSch = (C.HSV (A.Exp Float), C.HSV (A.Exp Float)) 

redBlue    = mkColourScheme (240, 1.0, 1.0) (0, 1.0, 1.0) 
blackWhite = mkColourScheme (0, 0, 1.0) (0, 0, 0)

mkColourScheme :: (Float, Float, Float)
                -> (Float, Float, Float) 
                -> ColourSch
mkColourScheme (h1, s1, v1) (h2, s2, v2) =
  let c1 = HSV (A.constant h1) (A.constant s1) (A.constant v1)
      c2 = HSV (A.constant h2) (A.constant s2) (A.constant v2)
  in (c1, c2)

interpolate :: ColourSch -> A.Exp Float -> A.Exp Colour
interpolate (HSV h1 s1 v1, HSV h2 s2 v2) pos =
  let h = linear h1 h2 pos
      s = linear s1 s2 pos
      v = linear v1 v2 pos
  in hsv h s v

linear :: (A.Elt t, Num (A.Exp t))
       => A.Exp t -> A.Exp t -> A.Exp t -> A.Exp t
linear a b s = a + s * (b - a)
