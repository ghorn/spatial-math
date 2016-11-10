{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}

module SpatialMath
       ( Euler(..)
       , ArcTan2(..)
       , rotateXyzAboutX
       , rotateXyzAboutY
       , rotateXyzAboutZ
       , euler321OfQuat
       , unsafeEuler321OfQuat
       , euler321OfDcm
       , unsafeEuler321OfDcm
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

-- | doesn't require RealFloat, used for overloading symbolics
class Floating a => ArcTan2 a where
  arctan2 :: a -> a -> a
instance ArcTan2 Double where arctan2 = atan2
instance ArcTan2 Float where arctan2 = atan2

-- $setup
-- |
-- >>> :{
--     let trunc :: Functor f => f Double -> f Double
--         trunc = fmap trunc'
--           where
--             trunc' x
--               | nearZero x = 0
--               | nearZero (x - 1) = 1
--               | nearZero (x + 1) = -1
--               | otherwise = x
-- :}

normalize' :: Floating a => Quaternion a -> Quaternion a
normalize' q = fmap (* normInv) q
  where
    normInv = 1/(norm q)

--normalize' :: (Floating a, Epsilon a) => Quaternion a -> Quaternion a
--normalize' = normalize

-- | Rotate a vector about the X axis
--
-- >>> trunc $ rotateXyzAboutX (V3 0 1 0) (pi/2)
-- V3 0.0 0.0 1.0
--
-- >>> trunc $ rotateXyzAboutX (V3 0 0 1) (pi/2)
-- V3 0.0 (-1.0) 0.0
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
-- >>> trunc $ rotateXyzAboutY (V3 0 0 1) (pi/2)
-- V3 1.0 0.0 0.0
--
-- >>> trunc $ rotateXyzAboutY (V3 1 0 0) (pi/2)
-- V3 0.0 0.0 (-1.0)
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
-- >>> trunc $ rotateXyzAboutZ (V3 1 0 0) (pi/2)
-- V3 0.0 1.0 0.0
--
-- >>> trunc $ rotateXyzAboutZ (V3 0 1 0) (pi/2)
-- V3 (-1.0) 0.0 0.0
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
euler321OfQuat :: (ArcTan2 a, Ord a) => Quaternion a -> Euler a
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

    yaw   = arctan2 r12 r11
    pitch = asin mr13
    roll  = arctan2 r23 r33

-- | Convert quaternion to Euler angles. Returns Nan if 2.0*(q1*q3 - q0*q2) is outside [-1, 1].
--
-- >>> unsafeEuler321OfQuat (Quaternion 1.0 (V3 0.0 0.0 0.0))
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 0.0}
--
-- >>> unsafeEuler321OfQuat (Quaternion (sqrt(2)/2) (V3 (sqrt(2)/2) 0.0 0.0))
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 1.5707963267948966}
--
-- >>> unsafeEuler321OfQuat (Quaternion (sqrt(2)/2) (V3 0.0 (sqrt(2)/2) 0.0))
-- Euler {eYaw = 0.0, ePitch = NaN, eRoll = 0.0}
--
-- >>> unsafeEuler321OfQuat (Quaternion (sqrt(2)/2) (V3 0.0 0.0 (sqrt(2)/2)))
-- Euler {eYaw = 1.5707963267948966, ePitch = -0.0, eRoll = 0.0}
--
unsafeEuler321OfQuat :: ArcTan2 a => Quaternion a -> Euler a
unsafeEuler321OfQuat (Quaternion q0 (V3 q1 q2 q3)) = Euler yaw pitch roll
  where
    r11 = q0*q0 + q1*q1 - q2*q2 - q3*q3
    r12 = 2.0*(q1*q2 + q0*q3)
    mr13 = -2.0*(q1*q3 - q0*q2)
    r23 = 2.0*(q2*q3 + q0*q1)
    r33 = q0*q0 - q1*q1 - q2*q2 + q3*q3

    yaw   = arctan2 r12 r11
    pitch = asin mr13
    roll  = arctan2 r23 r33

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
quatOfDcm :: (Floating a, Ord a) => M33 a -> Quaternion a
quatOfDcm
  (V3
    (V3 r11 r12 r13)
    (V3 r21 r22 r23)
    (V3 r31 r32 r33)
  )
  | r11 + r22 + r33 > 0 =
      let sqtrp1 = sqrt (r11 + r22 + r33 + 1)
          q0 = 0.5*sqtrp1
          qx = (r23 - r32)/(2.0*sqtrp1)
          qy = (r31 - r13)/(2.0*sqtrp1)
          qz = (r12 - r21)/(2.0*sqtrp1)
      in Quaternion q0 (V3 qx qy qz)
  | (r22 > r11) && (r22 > r33) =
      let -- max value at r22
          sqdip1' = sqrt (r22 - r11 - r33 + 1)

          qy = 0.5*sqdip1'

          sqdip1
            | sqdip1' == 0 = 0
            | otherwise = 0.5/sqdip1'

          q0 = (r31 - r13)*sqdip1
          qx = (r12 + r21)*sqdip1
          qz = (r23 + r32)*sqdip1

      in Quaternion q0 (V3 qx qy qz)
  | r33 > r11 =
      let -- max value at r33
          sqdip1' = sqrt (r33 - r11 - r22 + 1)

          qz = 0.5*sqdip1'

          sqdip1
            | sqdip1' == 0 = 0
            | otherwise = 0.5/sqdip1'

          q0 = (r12 - r21)*sqdip1
          qx = (r31 + r13)*sqdip1
          qy = (r23 + r32)*sqdip1

      in Quaternion q0 (V3 qx qy qz)
  | otherwise =
      let -- max value at r11
          sqdip1' = sqrt (r11 - r22 - r33 + 1)

          qx = 0.5*sqdip1'

          sqdip1
            | sqdip1' == 0 = 0
            | otherwise = 0.5/sqdip1'

          q0 = (r23 - r32)*sqdip1
          qy = (r12 + r21)*sqdip1
          qz = (r31 + r13)*sqdip1

      in Quaternion q0 (V3 qx qy qz)


quatOfDcmB2A :: (Floating a, Ord a) => M33 a -> Quaternion a
quatOfDcmB2A = quatConjugate . quatOfDcm

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
euler321OfDcm :: (Ord a, ArcTan2 a) => M33 a -> Euler a
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

    yaw   = arctan2 r12 r11
    pitch = asin mr13
    roll  = arctan2 r23 r33

-- | Convert DCM to euler angles. Returns Nan if r[1,3] is outside [-1, 1].
--
-- >>> unsafeEuler321OfDcm $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 0.0}
--
-- >>> unsafeEuler321OfDcm $ V3 (V3 0 1 0) (V3 (-1) 0 0) (V3 0 0 1)
-- Euler {eYaw = 1.5707963267948966, ePitch = -0.0, eRoll = 0.0}
--
-- >>> let s = sqrt(2)/2 in unsafeEuler321OfDcm $ V3 (V3 s s 0) (V3 (-s) s 0) (V3 0 0 1)
-- Euler {eYaw = 0.7853981633974483, ePitch = -0.0, eRoll = 0.0}
--
-- >>> unsafeEuler321OfDcm $ V3 (V3 0 0 1.1) (V3 0 0 0) (V3 0 0 0)
-- Euler {eYaw = 0.0, ePitch = NaN, eRoll = 0.0}
--
unsafeEuler321OfDcm :: ArcTan2 a => M33 a -> Euler a
unsafeEuler321OfDcm
  (V3
   (V3 r11 r12 r13)
   (V3   _   _ r23)
   (V3   _   _ r33)) = Euler yaw pitch roll
  where
    yaw   = arctan2 r12 r11
    pitch = asin (-r13)
    roll  = arctan2 r23 r33

-- | Convert Euler angles to quaternion. The scalar part of the result may be positive or negative.
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
quatOfEuler321 :: Floating a => Euler a -> Quaternion a
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

    q = Quaternion q0 (V3 q1 q2 q3)


-- | convert a quaternion to a DCM
--
-- >>> dcmOfQuat $ Quaternion 1.0 (V3 0.0 0.0 0.0)
-- V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)
--
-- >>> let s = sqrt(2)/2 in fmap trunc $ dcmOfQuat $ Quaternion s (V3 0.0 0.0 s)
-- V3 (V3 0.0 1.0 0.0) (V3 (-1.0) 0.0 0.0) (V3 0.0 0.0 1.0)
--
-- >>> dcmOfQuat $ Quaternion 0.9238795325112867 (V3 0.0 0.0 0.3826834323650898)
-- V3 (V3 0.7071067811865475 0.7071067811865476 0.0) (V3 (-0.7071067811865476) 0.7071067811865475 0.0) (V3 0.0 0.0 1.0)
--
dcmOfQuat :: Num a => Quaternion a -> M33 a
dcmOfQuat q = V3
              (V3 m11 m21 m31)
              (V3 m12 m22 m32)
              (V3 m13 m23 m33)
  where
    V3
      (V3 m11 m12 m13)
      (V3 m21 m22 m23)
      (V3 m31 m32 m33) = fromQuaternion q

-- | Convert DCM to euler angles
--
-- >>> fmap trunc $ dcmOfEuler321 $ Euler {eYaw = 0.0, ePitch = 0, eRoll = 0}
-- V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)
--
-- >>> fmap trunc $ dcmOfEuler321 $ Euler {eYaw = pi/2, ePitch = 0, eRoll = 0}
-- V3 (V3 0.0 1.0 0.0) (V3 (-1.0) 0.0 0.0) (V3 0.0 0.0 1.0)
--
-- >>> fmap trunc $ dcmOfEuler321 $ Euler {eYaw = pi/4, ePitch = 0, eRoll = 0}
-- V3 (V3 0.7071067811865476 0.7071067811865475 0.0) (V3 (-0.7071067811865475) 0.7071067811865476 0.0) (V3 0.0 0.0 1.0)
--
dcmOfEuler321 :: Floating a => Euler a -> M33 a
dcmOfEuler321 euler = dcm
  where
    cPs = cos (eYaw euler)
    sPs = sin (eYaw euler)
    cTh = cos (ePitch euler)
    sTh = sin (ePitch euler)
    cPh = cos (eRoll euler)
    sPh = sin (eRoll euler)

    dcm =
      V3
      (V3 (cTh*cPs) (cTh*sPs) (-sTh))
      (V3 (cPs*sTh*sPh - cPh*sPs) ( cPh*cPs + sTh*sPh*sPs) (cTh*sPh))
      (V3 (cPh*cPs*sTh + sPh*sPs) (-cPs*sPh + cPh*sTh*sPs) (cTh*cPh))

quatConjugate :: Num a => Quaternion a -> Quaternion a
quatConjugate (Quaternion q0 qv) = Quaternion q0 (fmap negate qv)

dcmOfQuatB2A :: Num a => Quaternion a -> M33 a
dcmOfQuatB2A = dcmOfQuat . quatConjugate

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
