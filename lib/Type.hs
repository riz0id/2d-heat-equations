module Type where

import Data.Array.Accelerate

type Conductivity = Float
type Entropy      = Float
type Timestep     = Float 

type Field t = Array DIM2 t
  
