{-# LANGUAGE ViewPatterns #-}

module Heat ( transport ) where

import Data.Array.Accelerate             as A
import Data.Array.Accelerate.LLVM.Native as CPU

import Type

transport :: Field Entropy
          -> Field Conductivity 
          -> Timestep
          -> Float -- dx 
          -> Float -- dy
          -> Field Entropy
transport hF qF (constant -> dt)
                (constant -> dx)
                (constant -> dy) =
  let kernel :: Stencil3x3 Float -> Stencil3x3 Float -> Exp Float
      kernel ((_, ht, _), (hl, hm, hr), (_, hb, _))
             (         _, ( _, qm,  _),          _) = 
        let dx2 = dx**2
            dy2 = dy**2
            sx  = qm*dt/dx2
            sy  = qm*dt/dy2
            pdx = hr-2*hm+hl
            pdy = ht-2*hm+hb
        in hm+sx*pdx+sy*pdy

      hBoundry = function $ \_ -> constant 0
      qBoundry = function $ \_ -> constant 2 
  in CPU.run $ A.stencil2 kernel hBoundry (use hF) qBoundry (use qF)
