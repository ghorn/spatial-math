{-# OPTIONS_GHC -Wall #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveDataTypeable #-}

module SpatialMath ( module Xyz
                   , module Quat
                   , Xyz(..)
                   , Quat(..)
                   , Euler(..)
                   , rotateXyzAboutX
                   , euler321OfQuat
                   , euler321OfDcm
                   , quatOfEuler321
                   , dcmOfQuat
                   , dcmOfQuatB2A
                   , quatOfDcm
                   , quatOfDcmB2A
                   , rotVecByDcm
                   , rotVecByDcmB2A
                   , rotVecByQuat
                   , rotVecByQuatB2A
                   ) where

import qualified Xyz
import qualified Quat
import Xyz ( Xyz(..) )
import Quat ( Quat(..) )

import Numeric.LinearAlgebra
import Foreign.Storable ( Storable )
import Data.Data ( Data )
import Data.Typeable ( Typeable1 )


data Euler a = Euler a a a deriving (Eq, Show)

deriving instance Typeable1 Euler
deriving instance Data a => Data (Euler a)

rotateXyzAboutX :: Floating a => Xyz a -> a -> Xyz a
rotateXyzAboutX (Xyz ax ay az) rotAngle = Xyz bx by bz
  where
    cosTheta = cos rotAngle
    sinTheta = sin rotAngle

    bx =  ax
    by =  ay*cosTheta + az*sinTheta
    bz = -ay*sinTheta + az*cosTheta

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

quatOfDcm :: (Storable a, RealFloat a) => Matrix a -> Quat a
quatOfDcm = quatOfEuler321 . euler321OfDcm

quatOfDcmB2A :: (Storable a, RealFloat a) => Matrix a -> Quat a
quatOfDcmB2A = Quat.inv . quatOfDcm

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
