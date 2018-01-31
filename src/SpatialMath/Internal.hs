{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}

module SpatialMath.Internal (
  libm_atan2
  , libm_atan2f
  ) where

import Foreign.C.Types ( CDouble(..), CFloat(..) )

foreign import ccall unsafe "math.h atan2" c_atan2
  :: CDouble -> CDouble -> CDouble

foreign import ccall unsafe "math.h atan2f" c_atan2f
  :: CFloat -> CFloat -> CFloat

libm_atan2 :: Double -> Double -> Double
libm_atan2 y x = ret
  where
    CDouble ret = c_atan2 (CDouble y) (CDouble x)

libm_atan2f :: Float -> Float -> Float
libm_atan2f y x = ret
  where
    CFloat ret = c_atan2f (CFloat y) (CFloat x)