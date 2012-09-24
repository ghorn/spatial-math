{-# OPTIONS_GHC -Wall #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveDataTypeable #-}

module Xyz ( Xyz(..)
           , zipWithXyz
           , cross
           , dot
           , normSquared
           , norm
           , distance
           , scale
           , normalizeTo
           , normalize
           , mult3x3ByXyz
           , mult3x3TransposeByXyz
           ) where

import Numeric.LinearAlgebra ( (@@>), Matrix )
import Foreign.Storable ( Storable )
import Data.Data ( Data )
import Data.Typeable ( Typeable1 )
import System.Random ( Random(..) )

data Xyz a = Xyz a a a deriving (Show, Eq)

deriving instance Typeable1 Xyz
deriving instance Data a => Data (Xyz a)

instance Functor Xyz where
  fmap f (Xyz x y z) = Xyz (f x) (f y) (f z)

instance Random a => Random (Xyz a) where
  random g0 = (Xyz x y z, gz)
    where
      (x,gx) = random g0
      (y,gy) = random gx
      (z,gz) = random gy
  randomR (Xyz x0 y0 z0, Xyz x1 y1 z1) g0 = (Xyz x y z, gz)
    where
      (x,gx) = randomR (x0,x1) g0
      (y,gy) = randomR (y0,y1) gx
      (z,gz) = randomR (z0,z1) gy

zipWithXyz :: (a -> b -> c) -> Xyz a -> Xyz b -> Xyz c
zipWithXyz f (Xyz x0 y0 z0) (Xyz x1 y1 z1) = Xyz (f x0 x1) (f y0 y1) (f z0 z1)

instance (Num a) => Num (Xyz a) where
  (+) = zipWithXyz (+)
  (-) = zipWithXyz (-)
  negate = fmap negate
  (*) = zipWithXyz (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger k = fmap fromInteger (Xyz k k k)

instance (Fractional a) => Fractional (Xyz a) where
  fromRational r = fmap fromRational (Xyz r r r)
  (/) = zipWithXyz (/)

instance (Floating a) => Floating (Xyz a) where
  pi = Xyz pi pi pi
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  tan   = fmap tan
  asin  = fmap asin
  acos  = fmap acos
  atan  = fmap atan
  sinh  = fmap sinh
  cosh  = fmap cosh
  tanh  = fmap tanh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

-- | c = a (cross) b
cross :: Num a => Xyz a -> Xyz a -> Xyz a
cross (Xyz ax ay az) (Xyz bx by bz) = Xyz cx cy cz
  where
    cx =   ay*bz - az*by
    cy = - ax*bz + az*bx
    cz =   ax*by - ay*bx

-- | c = a (dot) b
dot :: Num a => Xyz a -> Xyz a -> a
dot (Xyz ax ay az) (Xyz bx by bz) = ax*bx + ay*by + az*bz;

-- | c = vec (dot) vec
normSquared :: Num a => Xyz a -> a
normSquared x = dot x x

-- | norm(x)
norm :: Floating a => Xyz a -> a
norm x = sqrt $ dot x x

-- | norm(a - b)
distance :: Floating a => Xyz a -> Xyz a -> a
distance a b = norm $ a - b

-- | vec_out = vec_in*scale_factor
scale :: Num a => a -> Xyz a -> Xyz a
scale k = fmap (k *)

-- | vec_out = scale (new_norm/norm(vec_in)) vec_in
normalizeTo :: Floating a => a -> Xyz a -> Xyz a -> Xyz a
normalizeTo newNorm vec = scale (newNorm/(norm(vec) + 1e-12))

-- | vec_out = vec_in/norm(vec_in)
normalize :: Floating a => Xyz a -> Xyz a -> Xyz a
normalize = normalizeTo 1

-- | v_out = M*v
mult3x3ByXyz :: (Num a, Storable a) => Matrix a -> Xyz a -> Xyz a
mult3x3ByXyz mat (Xyz x y z) = Xyz x' y' z'
  where
    x' = (mat @@> (0,0))*x + (mat @@> (0,1))*y +  (mat @@> (0,2))*z
    y' = (mat @@> (1,0))*x + (mat @@> (1,1))*y +  (mat @@> (1,2))*z
    z' = (mat @@> (2,0))*x + (mat @@> (2,1))*y +  (mat @@> (2,2))*z

-- // v_out = M^T*v
mult3x3TransposeByXyz :: (Num a, Storable a) => Matrix a -> Xyz a -> Xyz a
mult3x3TransposeByXyz mat (Xyz x y z) = Xyz x' y' z'
  where
    x' = (mat @@> (0,0))*x + (mat @@> (1,0))*y +  (mat @@> (2,0))*z
    y' = (mat @@> (0,1))*x + (mat @@> (1,1))*y +  (mat @@> (2,1))*z
    z' = (mat @@> (0,2))*x + (mat @@> (1,2))*y +  (mat @@> (2,2))*z
