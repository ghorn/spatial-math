{-# OPTIONS_GHC -Wall #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveFunctor #-}

module SpatialMath ( module Xyz
                   , module Quat
                   , Xyz(..)
                   , Quat(..)
                   , Euler(..)
                   , rotateXyzAboutX
                   , rotateXyzAboutY
                   , rotateXyzAboutZ
                   , euler321OfQuat
                   , euler321OfDcm
                   , quatOfEuler321
                   , dcmOfQuat
                   , dcmOfQuatB2A
                   , dcmOfEuler321
                   , quatOfDcm
                   , quatOfDcmB2A
                   , rotVecByDcm
                   , rotVecByDcmB2A
                   , rotVecByQuat
                   , rotVecByQuatB2A
                   , rotVecByEuler
                   , rotVecByEulerB2A
                   ) where

import qualified Xyz
import qualified Quat
import Xyz ( Xyz(..) )
import Quat ( Quat(..) )

import Numeric.LinearAlgebra
import Foreign.Storable ( Storable )
import Data.Data ( Data )
import Data.Typeable ( Typeable1 )

-- | 3-2-1 Euler angle rotation sequence
data Euler a = Euler { eYaw :: a
                     , ePitch :: a
                     , eRoll :: a
                     } deriving (Eq, Show)

deriving instance Typeable1 Euler
deriving instance Data a => Data (Euler a)
deriving instance Functor Euler

-- | Rotate a vector about the X axis
--
-- >>> rotateXyzAboutX (Xyz 0 1 0) (pi/2)
-- Xyz 0.0 6.123233995736766e-17 1.0
--
-- >>> rotateXyzAboutX (Xyz 0 0 1) (pi/2)
-- Xyz 0.0 (-1.0) 6.123233995736766e-17
rotateXyzAboutX :: Floating a => Xyz a -> a -> Xyz a
rotateXyzAboutX (Xyz ax ay az) rotAngle = Xyz bx by bz
  where
    cosTheta = cos rotAngle
    sinTheta = sin rotAngle

    bx =  ax
    by =  ay*cosTheta - az*sinTheta
    bz =  ay*sinTheta + az*cosTheta

-- | Rotate a vector about the Y axis
--
-- >>> rotateXyzAboutY (Xyz 0 0 1) (pi/2)
-- Xyz 1.0 0.0 6.123233995736766e-17
--
-- >>> rotateXyzAboutY (Xyz 1 0 0) (pi/2)
-- Xyz 6.123233995736766e-17 0.0 (-1.0)
rotateXyzAboutY :: Floating a => Xyz a -> a -> Xyz a
rotateXyzAboutY (Xyz ax ay az) rotAngle = Xyz bx by bz
  where
    cosTheta = cos rotAngle
    sinTheta = sin rotAngle

    bx =  ax*cosTheta + az*sinTheta
    by =  ay
    bz = -ax*sinTheta + az*cosTheta

-- | Rotate a vector about the Z axis
--
-- >>> rotateXyzAboutZ (Xyz 1 0 0) (pi/2)
-- Xyz 6.123233995736766e-17 1.0 0.0
--
-- >>> rotateXyzAboutZ (Xyz 0 1 0) (pi/2)
-- Xyz (-1.0) 6.123233995736766e-17 0.0
--
rotateXyzAboutZ :: Floating a => Xyz a -> a -> Xyz a
rotateXyzAboutZ (Xyz ax ay az) rotAngle = Xyz bx by bz
  where
    cosTheta = cos rotAngle
    sinTheta = sin rotAngle

    bx =  ax*cosTheta - ay*sinTheta
    by =  ax*sinTheta + ay*cosTheta
    bz =  az


-- | Convert quaternion to Euler angles
--
-- >>> euler321OfQuat (Quat 1.0 0.0 0.0 0.0)
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 0.0}
--
-- >>> euler321OfQuat (Quat (sqrt(2)/2) (sqrt(2)/2) 0.0 0.0)
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 1.5707963267948966}
--
-- >>> euler321OfQuat (Quat (sqrt(2)/2) 0.0 (sqrt(2)/2) 0.0)
-- Euler {eYaw = 0.0, ePitch = 1.5707963267948966, eRoll = 0.0}
--
-- >>> euler321OfQuat (Quat (sqrt(2)/2) 0.0 0.0 (sqrt(2)/2))
-- Euler {eYaw = 1.5707963267948966, ePitch = -0.0, eRoll = 0.0}
--
euler321OfQuat :: RealFloat a => Quat a -> Euler a
euler321OfQuat (Quat q0 q1 q2 q3) = Euler yaw pitch roll
  where
    r11 = q0*q0 + q1*q1 - q2*q2 - q3*q3
    r12 = 2.0*(q1*q2 + q0*q3)
    mr13' = -2.0*(q1*q3 - q0*q2)
    mr13 -- nan protect
      | mr13' >  1 =  1
      | mr13' < -1 = -1
      | otherwise = mr13'
    r23 = 2.0*(q2*q3 + q0*q1)
    r33 = q0*q0 - q1*q1 - q2*q2 + q3*q3

    yaw   = atan2 r12 r11
    pitch = asin mr13
    roll  = atan2 r23 r33

-- | convert a DCM to a quaternion
--
-- >>> quatOfDcm $ fromLists [[1,0,0], [0,1,0], [0,0,1]]
-- Quat 1.0 0.0 0.0 0.0
--
-- >>> quatOfDcm $ fromLists [[0,1,0], [-1,0,0], [0,0,1]]
-- Quat 0.7071067811865476 0.0 0.0 0.7071067811865475
--
-- >>> let s = sqrt(2)/2 in quatOfDcm $ fromLists [[s,s,0], [-s,s,0], [0,0,1]]
-- Quat 0.9238795325112867 0.0 0.0 0.3826834323650898
--
quatOfDcm :: (Storable a, RealFloat a) => Matrix a -> Quat a
quatOfDcm = quatOfEuler321 . euler321OfDcm

quatOfDcmB2A :: (Storable a, RealFloat a) => Matrix a -> Quat a
quatOfDcmB2A = Quat.inv . quatOfDcm

-- | Convert DCM to euler angles
--
-- >>> euler321OfDcm $ fromLists [[1,0,0], [0,1,0], [0,0,1]]
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 0.0}
--
-- >>> euler321OfDcm $ fromLists [[0,1,0], [-1,0,0], [0,0,1]]
-- Euler {eYaw = 1.5707963267948966, ePitch = -0.0, eRoll = 0.0}
--
-- >>> let s = sqrt(2)/2 in euler321OfDcm $ fromLists [[s,s,0], [-s,s,0], [0,0,1]]
-- Euler {eYaw = 0.7853981633974483, ePitch = -0.0, eRoll = 0.0}
--
euler321OfDcm :: (RealFloat a, Storable a) => Matrix a -> Euler a
euler321OfDcm r = Euler yaw pitch roll
  where
    r11 = r @@> (0,0)
    r12 = r @@> (0,1)
    mr13' = -(r @@> (0,2))
    mr13 -- nan protect
      | mr13' >  1 =  1
      | mr13' < -1 = -1
      | otherwise = mr13'
    r23 = r @@> (1,2)
    r33 = r @@> (2,2)

    yaw   = atan2 r12 r11
    pitch = asin mr13
    roll  = atan2 r23 r33

-- | Convert Euler angles to quaternion
--
-- >>> quatOfEuler321 (Euler 0 0 0)
-- Quat 1.0 0.0 0.0 0.0
--
-- >>> quatOfEuler321 (Euler (pi/2) 0 0)
-- Quat 0.7071067811865476 0.0 0.0 0.7071067811865475
--
-- >>> quatOfEuler321 (Euler 0 (pi/2) 0)
-- Quat 0.7071067811865476 0.0 0.7071067811865475 0.0
--
-- >>> quatOfEuler321 (Euler 0 0 (pi/2))
-- Quat 0.7071067811865476 0.7071067811865475 0.0 0.0
--
quatOfEuler321 :: (Floating a, Ord a) => Euler a -> Quat a
quatOfEuler321 (Euler yaw pitch roll) = Quat.normalize q
  where
    sr2 = sin $ 0.5*roll
    cr2 = cos $ 0.5*roll
    sp2 = sin $ 0.5*pitch
    cp2 = cos $ 0.5*pitch
    sy2 = sin $ 0.5*yaw
    cy2 = cos $ 0.5*yaw
    q0 = cr2*cp2*cy2 + sr2*sp2*sy2
    q1 = sr2*cp2*cy2 - cr2*sp2*sy2
    q2 = cr2*sp2*cy2 + sr2*cp2*sy2
    q3 = cr2*cp2*sy2 - sr2*sp2*cy2

    q' = Quat q0 q1 q2 q3

    q
      | q0 < 0 = negate q'
      | otherwise = q'

-- | convert a quaternion to a DCM
--
-- >>> toLists $ dcmOfQuat $ Quat 1.0 0.0 0.0 0.0
-- [[1.0,0.0,0.0],[0.0,1.0,0.0],[0.0,0.0,1.0]]
--
-- >>> let s = sqrt(2)/2 in toLists $ dcmOfQuat $ Quat s 0.0 0.0 s
-- [[0.0,1.0000000000000002,0.0],[-1.0000000000000002,0.0,0.0],[0.0,0.0,1.0000000000000002]]
--
-- >>> toLists $ dcmOfQuat $ Quat 0.9238795325112867 0.0 0.0 0.3826834323650898
-- [[0.7071067811865475,0.7071067811865476,0.0],[-0.7071067811865476,0.7071067811865475,0.0],[0.0,0.0,1.0]]
--
dcmOfQuat :: (Num a, Element a) => Quat a -> Matrix a
dcmOfQuat (Quat q0 q1 q2 q3) = fromLists [ [r0, r1, r2]
                                         , [r3, r4, r5]
                                         , [r6, r7, r8]
                                         ]
  where
    -- 1st column
    r0 = q0*q0 + q1*q1 - q2*q2 - q3*q3
    r3 = 2*(q1*q2 - q0*q3)
    r6 = 2*(q1*q3 + q0*q2)

    -- 2nd column
    r1 = 2*(q1*q2 + q0*q3)
    r4 = q0*q0 - q1*q1 + q2*q2 - q3*q3
    r7 = 2*(q2*q3 - q0*q1)

    -- 3rd column
    r2 = 2*(q1*q3 - q0*q2)
    r5 = 2*(q2*q3 + q0*q1)
    r8 = q0*q0 - q1*q1 - q2*q2 + q3*q3

-- | Convert DCM to euler angles
--
-- >>> toLists $ dcmOfEuler321 $ Euler {eYaw = 0.0, ePitch = 0, eRoll = 0}
-- [[1.0,0.0,0.0],[0.0,1.0,0.0],[0.0,0.0,1.0]]
--
-- >>> toLists $ dcmOfEuler321 $ Euler {eYaw = pi/2, ePitch = 0, eRoll = 0}
-- [[2.220446049250313e-16,1.0,0.0],[-1.0,2.220446049250313e-16,0.0],[0.0,0.0,1.0]]
--
-- >>> toLists $ dcmOfEuler321 $ Euler {eYaw = pi/4, ePitch = 0, eRoll = 0}
-- [[0.7071067811865475,0.7071067811865476,0.0],[-0.7071067811865476,0.7071067811865475,0.0],[0.0,0.0,1.0]]
--
dcmOfEuler321 :: (Floating a, Element a, Ord a) => Euler a -> Matrix a
dcmOfEuler321 = dcmOfQuat . quatOfEuler321

dcmOfQuatB2A :: (Num a, Element a) => Quat a -> Matrix a
dcmOfQuatB2A = dcmOfQuat . Quat.inv

-- | vec_b = R_a2b * vec_a
rotVecByDcm :: (Num a, Storable a) => Matrix a -> Xyz a -> Xyz a
rotVecByDcm dcm vec = Xyz.mult3x3ByXyz dcm vec

-- | vec_a = R_a2b^T * vec_b
rotVecByDcmB2A :: (Num a, Storable a) => Matrix a -> Xyz a -> Xyz a
rotVecByDcmB2A dcm vec = Xyz.mult3x3TransposeByXyz dcm vec

-- | vec_b = q_a2b * vec_a * q_a2b^(-1)
--   vec_b = R(q_a2b) * vec_a
rotVecByQuat :: (Num a, Element a) => Quat a -> Xyz a -> Xyz a
rotVecByQuat q = rotVecByDcm (dcmOfQuat q)

rotVecByQuatB2A :: (Num a, Element a) => Quat a -> Xyz a -> Xyz a
rotVecByQuatB2A q = rotVecByDcmB2A (dcmOfQuat q)

rotVecByEuler :: (Floating a, Element a, Ord a) => Euler a -> Xyz a -> Xyz a
rotVecByEuler = rotVecByDcm . dcmOfEuler321

rotVecByEulerB2A :: (Floating a, Element a, Ord a) => Euler a -> Xyz a -> Xyz a
rotVecByEulerB2A = rotVecByDcmB2A . dcmOfEuler321
