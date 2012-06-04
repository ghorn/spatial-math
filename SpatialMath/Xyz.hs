{-# OPTIONS_GHC -Wall #-}

module SpatialMath.Xyz ( Xyz(..)
                       , zipWithXyz
                       , cross
                       , dot
                       , normSquared
                       , norm
                       , distance
                       , scale
                       , normalizeTo
                       , normalize
                       ) where


data Xyz a = Xyz a a a deriving (Show, Eq)

instance Functor Xyz where
  fmap f (Xyz x y z) = Xyz (f x) (f y) (f z)

zipWithXyz :: (a -> b -> c) -> Xyz a -> Xyz b -> Xyz c
zipWithXyz f (Xyz x0 y0 z0) (Xyz x1 y1 z1) = Xyz (f x0 x1) (f y0 y1) (f z0 z1)

instance (Num a) => Num (Xyz a) where
  (+) = zipWithXyz (+)
  (-) = zipWithXyz (-)
  negate = fmap negate
  (*) = error "(*) undefined for Xyz"
  abs = error "abs undefined for Xyz"
  signum = error "signum undefined for Xyz"
  fromInteger = error "fromInteger undefined for Xyz"

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

  
-- // v_out = M*v
-- // M is 3x3 row-major matrix
-- void
-- xyzMult3x3ByXyz (xyz_t * v_out, const double * const M, const xyz_t * const v)
-- {
--   v_out->x = M[0]*v->x +  M[1]*v->y +  M[2]*v->z;
--   v_out->y = M[3]*v->x +  M[4]*v->y +  M[5]*v->z;
--   v_out->z = M[6]*v->x +  M[7]*v->y +  M[8]*v->z;
-- }
-- 
-- // v_out = M^T*v
-- // M is 3x3 row-major matrix
-- void
-- xyz_mult_3x3_transpose_by_xyz(xyz_t * v_out, const double * const M, const xyz_t * const v)
-- {
--   v_out->x = M[0]*v->x +  M[3]*v->y +  M[6]*v->z;
--   v_out->y = M[1]*v->x +  M[4]*v->y +  M[7]*v->z;
--   v_out->z = M[2]*v->x +  M[5]*v->y +  M[8]*v->z;
-- }
