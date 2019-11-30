{-# LANGUAGE ViewPatterns #-}

module Config ( importBMP ) where

import qualified Prelude                       as P
import           Control.Monad 

import Data.Array.Accelerate                   as A
import Data.Array.Accelerate.IO.Codec.BMP      as A
import Data.Array.Accelerate.Data.Colour.RGBA  as A
import Data.Array.Accelerate.LLVM.Native       as CPU

import Type

importBMP :: P.FilePath -> Int -> Int -> Float -> P.IO (Field Float)
importBMP filepath width height (constant -> scale) = do
  arr <- P.either (error . P.show) P.id `fmap` readImageFromBMP filepath
  let Z :. h :. w =  arrayShape arr

  when (w P./= width P.|| h P./= height)
    $ error "accelerate-fluid: density-bmp does not match width x height"

  return . CPU.run $ map ((* scale) . luminance . unpackRGBA) (use arr)

