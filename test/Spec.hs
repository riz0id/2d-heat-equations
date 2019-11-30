{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

import Prelude                               as P

import Data.Array.Accelerate                 as A
import Data.Array.Accelerate.Data.Colour.HSV as C
import Data.Array.Accelerate.Data.Colour.RGB as C
import Data.Array.Accelerate.LLVM.Native     as CPU

import Graphics.Gloss
import Graphics.Gloss.Accelerate.Data.Picture
import Graphics.Gloss.Interface.IO.Game

import Config
import Heat
import Type
import Visualization

main :: IO ()
main = do
  let width  = 512
      height = 512
      
  hField <- importBMP "/home/syzygy/Documents/simulation/heat/2d/HeatSolver/samples/lena512.bmp" width height 100

  qField <- importBMP "/home/syzygy/Documents/simulation/heat/2d/HeatSolver/samples/heteroMat.bmp" width height 3
   
  let --physical params
      dimL   = 512
      dimH   = 512

      -- params
      dt     = (P.min dx dy)**2/3/4
      dx     = dimL/P.fromIntegral width
      dy     = dimH/P.fromIntegral height
      scheme = redBlue
  
      render hF = return $ heatMap hF scheme (1.0, 1.0)
      simulate hF = transport hF qField dt dx dy

  playIO
    (InWindow "heat test" (width+200, height+50) (10, 10))
    white
    140
    hField
    (render)
    (\_ -> return)
    (\elapsed hF -> do
        putStrLn $ "time elapsed: " P.++ show elapsed
        return . simulate $ hF)
  

