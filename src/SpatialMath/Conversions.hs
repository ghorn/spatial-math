{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}

module SpatialMath.Conversions
  ( rotateXyzAboutX
  , rotateXyzAboutY
  , rotateXyzAboutZ
  , euler321OfQuat
  , unsafeEuler321OfQuat
  , euler321OfDcm
  , unsafeEuler321OfDcm
  , quatOfEuler321
  , dcmOfQuat
  , dcmOfEuler321
  , quatOfDcm
  ) where

import Linear ( Quaternion(..), V3(..), fromQuaternion, norm, transpose )

import SpatialMath.ArcTan2 ( ArcTan2(..) )
import SpatialMath.Types ( Dcm(..), Euler(..), Quat(..), V3T(..), fromV3 )

-- $setup
-- >>> import Linear ( M33, nearZero )
-- >>> import SpatialMath.Types ( toV3 )
-- >>> let toM33 = fmap toV3 . toV3 . getUnitVectors :: Dcm f g a -> M33 a
-- >>> let fromM33 = DcmUnitVectors . fromV3 . fmap fromV3 :: M33 a -> Dcm f g a
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


normalize' :: Floating a => Quat f g a -> Quat f g a
normalize' (Quat q) = Quat $ fmap (* normInv) q
  where
    normInv = 1/(norm q)

--normalize' :: (Floating a, Epsilon a) => Quaternion a -> Quaternion a
--normalize' = normalize


-- | Rotate a vector about the X axis
--
-- >>> trunc $ rotateXyzAboutX (V3T (V3 0 1 0)) (pi/2)
-- V3T {unV = V3 0.0 0.0 1.0}
--
-- >>> trunc $ rotateXyzAboutX (V3T (V3 0 0 1)) (pi/2)
-- V3T {unV = V3 0.0 (-1.0) 0.0}
rotateXyzAboutX :: Floating a => V3T f a -> a -> V3T f a
rotateXyzAboutX (V3T (V3 ax ay az)) rotAngle = V3T (V3 bx by bz)
  where
    cosTheta = cos rotAngle
    sinTheta = sin rotAngle

    bx =  ax
    by =  ay*cosTheta - az*sinTheta
    bz =  ay*sinTheta + az*cosTheta


-- | Rotate a vector about the Y axis
--
-- >>> trunc $ rotateXyzAboutY (V3T (V3 0 0 1)) (pi/2)
-- V3T {unV = V3 1.0 0.0 0.0}
--
-- >>> trunc $ rotateXyzAboutY (V3T (V3 1 0 0)) (pi/2)
-- V3T {unV = V3 0.0 0.0 (-1.0)}
rotateXyzAboutY :: Floating a => V3T f a -> a -> V3T f a
rotateXyzAboutY (V3T (V3 ax ay az)) rotAngle = V3T (V3 bx by bz)
  where
    cosTheta = cos rotAngle
    sinTheta = sin rotAngle

    bx =  ax*cosTheta + az*sinTheta
    by =  ay
    bz = -ax*sinTheta + az*cosTheta


-- | Rotate a vector about the Z axis
--
-- >>> trunc $ rotateXyzAboutZ (V3T (V3 1 0 0)) (pi/2)
-- V3T {unV = V3 0.0 1.0 0.0}
--
-- >>> trunc $ rotateXyzAboutZ (V3T (V3 0 1 0)) (pi/2)
-- V3T {unV = V3 (-1.0) 0.0 0.0}
--
rotateXyzAboutZ :: Floating a => V3T f a -> a -> V3T f a
rotateXyzAboutZ (V3T (V3 ax ay az)) rotAngle = V3T (V3 bx by bz)
  where
    cosTheta = cos rotAngle
    sinTheta = sin rotAngle

    bx =  ax*cosTheta - ay*sinTheta
    by =  ax*sinTheta + ay*cosTheta
    bz =  az


-- | Convert quaternion to Euler angles
--
-- >>> euler321OfQuat (Quat (Quaternion 1.0 (V3 0.0 0.0 0.0)))
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 0.0}
--
-- >>> euler321OfQuat (Quat (Quaternion (sqrt(2)/2) (V3 (sqrt(2)/2) 0.0 0.0)))
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 1.5707963267948966}
--
-- >>> euler321OfQuat (Quat (Quaternion (sqrt(2)/2) (V3 0.0 (sqrt(2)/2) 0.0)))
-- Euler {eYaw = 0.0, ePitch = 1.5707963267948966, eRoll = 0.0}
--
-- >>> euler321OfQuat (Quat (Quaternion (sqrt(2)/2) (V3 0.0 0.0 (sqrt(2)/2))))
-- Euler {eYaw = 1.5707963267948966, ePitch = -0.0, eRoll = 0.0}
--
euler321OfQuat :: (ArcTan2 a, Ord a) => Quat f g a -> Euler f g a
euler321OfQuat (Quat (Quaternion q0 (V3 q1 q2 q3))) = Euler yaw pitch roll
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
-- >>> unsafeEuler321OfQuat (Quat (Quaternion 1.0 (V3 0.0 0.0 0.0)))
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 0.0}
--
-- >>> unsafeEuler321OfQuat (Quat (Quaternion (sqrt(2)/2) (V3 (sqrt(2)/2) 0.0 0.0)))
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 1.5707963267948966}
--
-- >>> unsafeEuler321OfQuat (Quat (Quaternion (sqrt(2)/2) (V3 0.0 (sqrt(2)/2) 0.0)))
-- Euler {eYaw = 0.0, ePitch = NaN, eRoll = 0.0}
--
-- >>> unsafeEuler321OfQuat (Quat (Quaternion (sqrt(2)/2) (V3 0.0 0.0 (sqrt(2)/2))))
-- Euler {eYaw = 1.5707963267948966, ePitch = -0.0, eRoll = 0.0}
--
unsafeEuler321OfQuat :: ArcTan2 a => Quat f g a -> Euler f g a
unsafeEuler321OfQuat (Quat (Quaternion q0 (V3 q1 q2 q3))) = Euler yaw pitch roll
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
-- >>> quatOfDcm $ fromM33 $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
-- Quat {unQuat = Quaternion 1.0 (V3 0.0 0.0 0.0)}
--
-- >>> quatOfDcm $ fromM33 $ V3 (V3 0 1 0) (V3 (-1) 0 0) (V3 0 0 1)
-- Quat {unQuat = Quaternion 0.7071067811865476 (V3 0.0 0.0 0.7071067811865475)}
--
-- >>> let s = sqrt(2)/2 in quatOfDcm $ fromM33 $ V3 (V3 s s 0) (V3 (-s) s 0) (V3 0 0 1)
-- Quat {unQuat = Quaternion 0.9238795325112867 (V3 0.0 0.0 0.3826834323650898)}
quatOfDcm :: (Floating a, Ord a) => Dcm f g a -> Quat f g a
quatOfDcm
  (DcmUnitVectors
   (V3T (V3
    (V3T (V3 r11 r12 r13))
    (V3T (V3 r21 r22 r23))
    (V3T (V3 r31 r32 r33)))))
  | r11 + r22 + r33 > 0 =
      let sqtrp1 = sqrt (r11 + r22 + r33 + 1)
          q0 = 0.5*sqtrp1
          qx = (r23 - r32)/(2.0*sqtrp1)
          qy = (r31 - r13)/(2.0*sqtrp1)
          qz = (r12 - r21)/(2.0*sqtrp1)
      in Quat $ Quaternion q0 (V3 qx qy qz)
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

      in Quat $ Quaternion q0 (V3 qx qy qz)
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

      in Quat $ Quaternion q0 (V3 qx qy qz)
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

      in Quat $ Quaternion q0 (V3 qx qy qz)


-- | Convert DCM to euler angles
--
-- >>> euler321OfDcm $ fromM33 $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 0.0}
--
-- >>> euler321OfDcm $ fromM33 $ V3 (V3 0 1 0) (V3 (-1) 0 0) (V3 0 0 1)
-- Euler {eYaw = 1.5707963267948966, ePitch = -0.0, eRoll = 0.0}
--
-- >>> let s = sqrt(2)/2 in euler321OfDcm $ fromM33 $ V3 (V3 s s 0) (V3 (-s) s 0) (V3 0 0 1)
-- Euler {eYaw = 0.7853981633974483, ePitch = -0.0, eRoll = 0.0}
--
euler321OfDcm :: (Ord a, ArcTan2 a) => Dcm f g a -> Euler f g a
euler321OfDcm
  (DcmUnitVectors
   (V3T (V3
    (V3T (V3 r11 r12 r13))
    (V3T (V3   _   _ r23))
    (V3T (V3   _   _ r33))))) = Euler yaw pitch roll
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
-- >>> unsafeEuler321OfDcm $ fromM33 $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 0.0}
--
-- >>> unsafeEuler321OfDcm $ fromM33 $ V3 (V3 0 1 0) (V3 (-1) 0 0) (V3 0 0 1)
-- Euler {eYaw = 1.5707963267948966, ePitch = -0.0, eRoll = 0.0}
--
-- >>> let s = sqrt(2)/2 in unsafeEuler321OfDcm $ fromM33 $ V3 (V3 s s 0) (V3 (-s) s 0) (V3 0 0 1)
-- Euler {eYaw = 0.7853981633974483, ePitch = -0.0, eRoll = 0.0}
--
-- >>> unsafeEuler321OfDcm $ fromM33 $ V3 (V3 0 0 1.1) (V3 0 0 0) (V3 0 0 0)
-- Euler {eYaw = 0.0, ePitch = NaN, eRoll = 0.0}
--
unsafeEuler321OfDcm :: ArcTan2 a => Dcm f g a -> Euler f g a
unsafeEuler321OfDcm
  (DcmUnitVectors
   (V3T (V3
    (V3T (V3 r11 r12 r13))
    (V3T (V3   _   _ r23))
    (V3T (V3   _   _ r33))))) = Euler yaw pitch roll
  where
    yaw   = arctan2 r12 r11
    pitch = asin (-r13)
    roll  = arctan2 r23 r33


-- | Convert Euler angles to quaternion. The scalar part of the result may be positive or negative.
--
-- >>> quatOfEuler321 (Euler 0 0 0)
-- Quat {unQuat = Quaternion 1.0 (V3 0.0 0.0 0.0)}
--
-- >>> quatOfEuler321 (Euler (pi/2) 0 0)
-- Quat {unQuat = Quaternion 0.7071067811865476 (V3 0.0 0.0 0.7071067811865475)}
--
-- >>> quatOfEuler321 (Euler 0 (pi/2) 0)
-- Quat {unQuat = Quaternion 0.7071067811865476 (V3 0.0 0.7071067811865475 0.0)}
--
-- >>> quatOfEuler321 (Euler 0 0 (pi/2))
-- Quat {unQuat = Quaternion 0.7071067811865476 (V3 0.7071067811865475 0.0 0.0)}
--
quatOfEuler321 :: Floating a => Euler f g a -> Quat f g a
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

    q = Quat $ Quaternion q0 (V3 q1 q2 q3)


-- | convert a quaternion to a DCM
--
-- >>> toM33 $ dcmOfQuat $ Quat $ Quaternion 1.0 (V3 0.0 0.0 0.0)
-- V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)
--
-- >>> let s = sqrt(2)/2 in toM33 $ trunc $ dcmOfQuat $ Quat $ Quaternion s (V3 0.0 0.0 s)
-- V3 (V3 0.0 1.0 0.0) (V3 (-1.0) 0.0 0.0) (V3 0.0 0.0 1.0)
--
-- >>> toM33 $ dcmOfQuat $ Quat $ Quaternion 0.9238795325112867 (V3 0.0 0.0 0.3826834323650898)
-- V3 (V3 0.7071067811865475 0.7071067811865476 0.0) (V3 (-0.7071067811865476) 0.7071067811865475 0.0) (V3 0.0 0.0 1.0)
--
dcmOfQuat :: Num a => Quat f g a -> Dcm f g a
dcmOfQuat = DcmUnitVectors . fromV3 . transpose . fromV3 . fromQuaternion . unQuat


-- | Convert DCM to euler angles
--
-- >>> toM33 $ trunc $ dcmOfEuler321 $ Euler {eYaw = 0.0, ePitch = 0, eRoll = 0}
-- V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)
--
-- >>> toM33 $ trunc $ dcmOfEuler321 $ Euler {eYaw = pi/2, ePitch = 0, eRoll = 0}
-- V3 (V3 0.0 1.0 0.0) (V3 (-1.0) 0.0 0.0) (V3 0.0 0.0 1.0)
--
-- >>> toM33 $ trunc $ dcmOfEuler321 $ Euler {eYaw = pi/4, ePitch = 0, eRoll = 0}
-- V3 (V3 0.7071067811865476 0.7071067811865475 0.0) (V3 (-0.7071067811865475) 0.7071067811865476 0.0) (V3 0.0 0.0 1.0)
--
dcmOfEuler321 :: Floating a => Euler f g a -> Dcm f g a
dcmOfEuler321 euler = DcmUnitVectors dcm
  where
    cPs = cos (eYaw euler)
    sPs = sin (eYaw euler)
    cTh = cos (ePitch euler)
    sTh = sin (ePitch euler)
    cPh = cos (eRoll euler)
    sPh = sin (eRoll euler)

    dcm =
      V3T $ V3
      (V3T (V3 (cTh*cPs) (cTh*sPs) (-sTh)))
      (V3T (V3 (cPs*sTh*sPh - cPh*sPs) ( cPh*cPs + sTh*sPh*sPs) (cTh*sPh)))
      (V3T (V3 (cPh*cPs*sTh + sPh*sPs) (-cPs*sPh + cPh*sTh*sPs) (cTh*cPh)))
