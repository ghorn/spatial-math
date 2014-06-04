{-# OPTIONS_GHC -Wall #-}

module SpatialMath ( Euler(..)
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
                     -- * re-exported from linear
                   , M33
                   , V3(..)
                   , Quaternion(..)
                   ) where

import Linear

import Types

normalize' :: Floating a => Quaternion a -> Quaternion a
normalize' q = fmap (* normInv) q
  where
    normInv = 1/(norm q)

--normalize' :: (Floating a, Epsilon a) => Quaternion a -> Quaternion a
--normalize' = normalize

-- | Rotate a vector about the X axis
--
-- >>> rotateXyzAboutX (V3 0 1 0) (pi/2)
-- V3 0.0 6.123233995736766e-17 1.0
--
-- >>> rotateXyzAboutX (V3 0 0 1) (pi/2)
-- V3 0.0 (-1.0) 6.123233995736766e-17
rotateXyzAboutX :: Floating a => V3 a -> a -> V3 a
rotateXyzAboutX (V3 ax ay az) rotAngle = V3 bx by bz
  where
    cosTheta = cos rotAngle
    sinTheta = sin rotAngle

    bx =  ax
    by =  ay*cosTheta - az*sinTheta
    bz =  ay*sinTheta + az*cosTheta

-- | Rotate a vector about the Y axis
--
-- >>> rotateXyzAboutY (V3 0 0 1) (pi/2)
-- V3 1.0 0.0 6.123233995736766e-17
--
-- >>> rotateXyzAboutY (V3 1 0 0) (pi/2)
-- V3 6.123233995736766e-17 0.0 (-1.0)
rotateXyzAboutY :: Floating a => V3 a -> a -> V3 a
rotateXyzAboutY (V3 ax ay az) rotAngle = V3 bx by bz
  where
    cosTheta = cos rotAngle
    sinTheta = sin rotAngle

    bx =  ax*cosTheta + az*sinTheta
    by =  ay
    bz = -ax*sinTheta + az*cosTheta

-- | Rotate a vector about the Z axis
--
-- >>> rotateXyzAboutZ (V3 1 0 0) (pi/2)
-- V3 6.123233995736766e-17 1.0 0.0
--
-- >>> rotateXyzAboutZ (V3 0 1 0) (pi/2)
-- V3 (-1.0) 6.123233995736766e-17 0.0
--
rotateXyzAboutZ :: Floating a => V3 a -> a -> V3 a
rotateXyzAboutZ (V3 ax ay az) rotAngle = V3 bx by bz
  where
    cosTheta = cos rotAngle
    sinTheta = sin rotAngle

    bx =  ax*cosTheta - ay*sinTheta
    by =  ax*sinTheta + ay*cosTheta
    bz =  az


-- | Convert quaternion to Euler angles
--
-- >>> euler321OfQuat (Quaternion 1.0 (V3 0.0 0.0 0.0))
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 0.0}
--
-- >>> euler321OfQuat (Quaternion (sqrt(2)/2) (V3 (sqrt(2)/2) 0.0 0.0))
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 1.5707963267948966}
--
-- >>> euler321OfQuat (Quaternion (sqrt(2)/2) (V3 0.0 (sqrt(2)/2) 0.0))
-- Euler {eYaw = 0.0, ePitch = 1.5707963267948966, eRoll = 0.0}
--
-- >>> euler321OfQuat (Quaternion (sqrt(2)/2) (V3 0.0 0.0 (sqrt(2)/2)))
-- Euler {eYaw = 1.5707963267948966, ePitch = -0.0, eRoll = 0.0}
--
euler321OfQuat :: RealFloat a => Quaternion a -> Euler a
euler321OfQuat (Quaternion q0 (V3 q1 q2 q3)) = Euler yaw pitch roll
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
-- >>> quatOfDcm $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
-- Quaternion 1.0 (V3 0.0 0.0 0.0)
--
-- >>> quatOfDcm $ V3 (V3 0 1 0) (V3 (-1) 0 0) (V3 0 0 1)
-- Quaternion 0.7071067811865476 (V3 0.0 0.0 0.7071067811865475)
--
-- >>> let s = sqrt(2)/2 in quatOfDcm $ V3 (V3 s s 0) (V3 (-s) s 0) (V3 0 0 1)
-- Quaternion 0.9238795325112867 (V3 0.0 0.0 0.3826834323650898)
--
quatOfDcm :: RealFloat a => M33 a -> Quaternion a
quatOfDcm = quatOfEuler321 . euler321OfDcm

quatOfDcmB2A :: (Conjugate a, RealFloat a) => M33 a -> Quaternion a
quatOfDcmB2A = conjugate . quatOfDcm

-- | Convert DCM to euler angles
--
-- >>> euler321OfDcm $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 0.0}
--
-- >>> euler321OfDcm $ V3 (V3 0 1 0) (V3 (-1) 0 0) (V3 0 0 1)
-- Euler {eYaw = 1.5707963267948966, ePitch = -0.0, eRoll = 0.0}
--
-- >>> let s = sqrt(2)/2 in euler321OfDcm $ V3 (V3 s s 0) (V3 (-s) s 0) (V3 0 0 1)
-- Euler {eYaw = 0.7853981633974483, ePitch = -0.0, eRoll = 0.0}
--
euler321OfDcm :: RealFloat a => M33 a -> Euler a
euler321OfDcm
  (V3
   (V3 r11 r12 r13)
   (V3   _   _ r23)
   (V3   _   _ r33)) = Euler yaw pitch roll
  where
    mr13' = -r13
    mr13 -- nan protect
      | mr13' >  1 =  1
      | mr13' < -1 = -1
      | otherwise = mr13'

    yaw   = atan2 r12 r11
    pitch = asin mr13
    roll  = atan2 r23 r33

-- | Convert Euler angles to quaternion
--
-- >>> quatOfEuler321 (Euler 0 0 0)
-- Quaternion 1.0 (V3 0.0 0.0 0.0)
--
-- >>> quatOfEuler321 (Euler (pi/2) 0 0)
-- Quaternion 0.7071067811865476 (V3 0.0 0.0 0.7071067811865475)
--
-- >>> quatOfEuler321 (Euler 0 (pi/2) 0)
-- Quaternion 0.7071067811865476 (V3 0.0 0.7071067811865475 0.0)
--
-- >>> quatOfEuler321 (Euler 0 0 (pi/2))
-- Quaternion 0.7071067811865476 (V3 0.7071067811865475 0.0 0.0)
--
quatOfEuler321 :: (Floating a, Ord a) => Euler a -> Quaternion a
quatOfEuler321 (Euler yaw pitch roll) = normalize' q
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

    q' = Quaternion q0 (V3 q1 q2 q3)

    q
      | q0 < 0 = Quaternion (-q0) (V3 (-q1) (-q2) (-q3))
      | otherwise = q'

-- | convert a quaternion to a DCM
--
-- >>> dcmOfQuat $ Quaternion 1.0 (V3 0.0 0.0 0.0)
-- V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)
--
-- >>> let s = sqrt(2)/2 in dcmOfQuat $ Quaternion s (V3 0.0 0.0 s)
-- V3 (V3 0.0 1.0000000000000002 0.0) (V3 (-1.0000000000000002) 0.0 0.0) (V3 0.0 0.0 1.0000000000000002)
--
-- >>> dcmOfQuat $ Quaternion 0.9238795325112867 (V3 0.0 0.0 0.3826834323650898)
-- V3 (V3 0.7071067811865475 0.7071067811865476 0.0) (V3 (-0.7071067811865476) 0.7071067811865475 0.0) (V3 0.0 0.0 1.0)
--
dcmOfQuat :: Num a => Quaternion a -> M33 a
dcmOfQuat (Quaternion q0 (V3 q1 q2 q3)) = V3 (V3 r0 r1 r2)
                                             (V3 r3 r4 r5)
                                             (V3 r6 r7 r8)
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
-- >>> dcmOfEuler321 $ Euler {eYaw = 0.0, ePitch = 0, eRoll = 0}
-- V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)
--
-- >>> dcmOfEuler321 $ Euler {eYaw = pi/2, ePitch = 0, eRoll = 0}
-- V3 (V3 2.220446049250313e-16 1.0 0.0) (V3 (-1.0) 2.220446049250313e-16 0.0) (V3 0.0 0.0 1.0)
--
-- >>> dcmOfEuler321 $ Euler {eYaw = pi/4, ePitch = 0, eRoll = 0}
-- V3 (V3 0.7071067811865475 0.7071067811865476 0.0) (V3 (-0.7071067811865476) 0.7071067811865475 0.0) (V3 0.0 0.0 1.0)
--
dcmOfEuler321 :: (Floating a, Ord a) => Euler a -> M33 a
dcmOfEuler321 = dcmOfQuat . quatOfEuler321

dcmOfQuatB2A :: (Conjugate a, RealFloat a) => Quaternion a -> M33 a
dcmOfQuatB2A = dcmOfQuat . conjugate

-- | vec_b = R_a2b * vec_a
rotVecByDcm :: Num a => M33 a -> V3 a -> V3 a
rotVecByDcm dcm vec = dcm !* vec

-- | vec_a = R_a2b^T * vec_b
rotVecByDcmB2A :: Num a => M33 a -> V3 a -> V3 a
rotVecByDcmB2A dcm vec = vec *! dcm

-- | vec_b = q_a2b * vec_a * q_a2b^(-1)
--   vec_b = R(q_a2b) * vec_a
rotVecByQuat :: Num a => Quaternion a -> V3 a -> V3 a
rotVecByQuat q = rotVecByDcm (dcmOfQuat q)

rotVecByQuatB2A :: Num a => Quaternion a -> V3 a -> V3 a
rotVecByQuatB2A q = rotVecByDcmB2A (dcmOfQuat q)

rotVecByEuler :: (Floating a, Ord a) => Euler a -> V3 a -> V3 a
rotVecByEuler = rotVecByDcm . dcmOfEuler321

rotVecByEulerB2A :: (Floating a, Ord a) => Euler a -> V3 a -> V3 a
rotVecByEulerB2A = rotVecByDcmB2A . dcmOfEuler321
