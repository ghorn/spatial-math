{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}

module SpatialMath
  ( ArcTan2(..)
  , Dcm(..)
  , Euler(..)
  , Quat(..)
  , Quaternion(..)
  , V3T(..), V3(..)
  , R1(..), R2(..), R3(..)
  , cross
  , orthonormalize
  , dcmOfQuat
  , dcmOfEuler321
  , quatOfDcm
  , quatOfEuler321
  , euler321OfDcm
  , unsafeEuler321OfDcm
  , euler321OfQuat
  , unsafeEuler321OfQuat
  , rotVecByDcm

  , transposeDcm, composeDcm
  , qmult, qtranspose

  , identity, qidentity
  ) where

import Linear hiding ( cross, identity, normalize, transpose )
import qualified Linear as L

import SpatialMath.ArcTan2 ( ArcTan2(..) )
import SpatialMath.Conversions
import SpatialMath.Types ( Dcm(..), Euler(..), Quat(..), V3T(..), toV3, fromV3 )


cross :: Num a => V3T f a -> V3T f a -> V3T f a
cross x y = fromV3 (toV3 x `L.cross` toV3 y)

transposeDcm :: Dcm f1 f2 a -> Dcm f2 f1 a
transposeDcm = DcmUnitVectors . L.transpose . getUnitVectors

composeDcm :: Num a => Dcm f1 f2 a -> Dcm f2 f3 a -> Dcm f1 f3 a
composeDcm (DcmUnitVectors dcm_a2b) (DcmUnitVectors dcm_b2c) = DcmUnitVectors (dcm_b2c !*! dcm_a2b)

-- | vec_b = R_a2b * vec_a
rotVecByDcm :: Num a => Dcm f1 f2 a -> V3T f1 a -> V3T f2 a
rotVecByDcm (DcmUnitVectors dcm) vec = dcm !* vec

identity :: Num a => Dcm f f a
identity =
  DcmUnitVectors $ V3T $ V3
  (V3T (V3 1 0 0))
  (V3T (V3 0 1 0))
  (V3T (V3 0 0 1))

qidentity :: Num a => Quat f f a
qidentity = Quat (Quaternion 1 (pure 0))

qmult :: forall a f1 f2 f3 . Num a => Quat f1 f2 a -> Quat f2 f3 a -> Quat f2 f3 a
qmult (Quat q_a2b) (Quat q_b2c) = Quat (q_a2b `quatMult'` q_b2c)
  where
    quatMult' :: Quaternion a -> Quaternion a -> Quaternion a
    quatMult' (Quaternion s1 v1) (Quaternion s2 v2) =
      Quaternion (s1*s2 - (v1 `dot` v2)) $
      (v1 `L.cross` v2) + s1*^v2 + s2*^v1

qtranspose :: Num a => Quat f1 f2 a -> Quat f2 f1 a
qtranspose (Quat (Quaternion q0 qv)) = Quat $ Quaternion q0 (fmap negate qv)



orthonormalize :: Floating a => Dcm f1 f2 a -> Dcm f1 f2 a
orthonormalize
  (DcmUnitVectors
    (V3T (V3
     (V3T (V3 m00 m01 m02))
     (V3T (V3 m10 m11 m12))
     (V3T (V3 m20 m21 m22))))) = DcmUnitVectors ret
  where
    -- compute q0
    fInvLength0 = 1.0/sqrt(m00*m00 + m10*m10 + m20*m20)

    m00' = m00*fInvLength0
    m10' = m10*fInvLength0
    m20' = m20*fInvLength0

    -- compute q1
    fDot0' = m00'*m01 + m10'*m11 + m20'*m21

    m01' = m01 - fDot0'*m00'
    m11' = m11 - fDot0'*m10'
    m21' = m21 - fDot0'*m20'

    fInvLength1 = 1.0/sqrt(m01'*m01' + m11'*m11' + m21'*m21')

    m01'' = m01' * fInvLength1
    m11'' = m11' * fInvLength1
    m21'' = m21' * fInvLength1

    -- compute q2
    fDot1 = m01''*m02 + m11''*m12 + m21''*m22
    fDot0 = m00'*m02 + m10'*m12 + m20'*m22

    m02' = m02 - (fDot0*m00' + fDot1*m01'')
    m12' = m12 - (fDot0*m10' + fDot1*m11'')
    m22' = m22 - (fDot0*m20' + fDot1*m21'')

    fInvLength2 = 1.0/sqrt(m02'*m02' + m12'*m12' + m22'*m22')

    m02'' = m02' * fInvLength2
    m12'' = m12' * fInvLength2
    m22'' = m22' * fInvLength2

    ret =
      V3T (V3
      (V3T (V3 m00' m01'' m02''))
      (V3T (V3 m10' m11'' m12''))
      (V3T (V3 m20' m21'' m22'')))
