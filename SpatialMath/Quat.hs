{-# OPTIONS_GHC -Wall #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveDataTypeable #-}

module SpatialMath.Quat ( Quat(..)
                        , zipWithQuat
                        , inv
                        , norm
                        , normalize
                        , qmult
                        , qmult'
                        ) where

import Data.Data ( Data )
import Data.Typeable ( Typeable1 )

data Quat a = Quat a a a a deriving (Show, Eq)

deriving instance Typeable1 Quat
deriving instance Data a => Data (Quat a)

instance Functor Quat where
  fmap f (Quat q0 q1 q2 q3) = Quat (f q0) (f q1) (f q2) (f q3)

zipWithQuat :: (a -> b -> c) -> Quat a -> Quat b -> Quat c
zipWithQuat f (Quat p0 p1 p2 p3) (Quat q0 q1 q2 q3) = Quat (f p0 q0) (f p1 q1) (f p2 q2) (f p3 q3)

instance (Num a, Ord a) => Num (Quat a) where
  (+) = zipWithQuat (+)
  (-) = zipWithQuat (-)
  negate = fmap negate
  (*) = qmult
  abs = fmap abs
  signum = fmap signum
  fromInteger = error "fromInteger undefined for Quat"

-- | q_out = q_in^-1
inv :: Num a => Quat a -> Quat a
inv (Quat q0 q1 q2 q3) = Quat q0 (-q1) (-q2) (-q3)

-- | return ||q||
norm :: Floating a => Quat a -> a
norm (Quat q0 q1 q2 q3) = sqrt $ q0*q0 + q1*q1 + q2*q2 + q3*q3

-- | q /= ||q||
normalize :: Floating a => Quat a -> Quat a
normalize q = fmap (* normInv) q
  where
    normInv = 1/(norm q)

-- | quaternion multiply: qa * qb
qmult :: (Num a, Ord a) => Quat a -> Quat a -> Quat a
qmult (Quat p0 p1 p2 p3) (Quat q0 q1 q2 q3)
  | r0 < 0 = negate qOut
  | otherwise = qOut
  where
    qOut = Quat r0 r1 r2 r3
    r0 = p0*q0 - p1*q1 - p2*q2 - p3*q3
    r1 = p0*q1 + p1*q0 + p2*q3 - p3*q2
    r2 = p0*q2 - p1*q3 + p2*q0 + p3*q1
    r3 = p0*q3 + p1*q2 - p2*q1 + p3*q0

-- | quaternion multiply then normalize
qmult' :: (Floating a, Ord a) => Quat a -> Quat a -> Quat a
qmult' p q = normalize (qmult q p)
