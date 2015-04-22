{-# OPTIONS_GHC -Wall #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveGeneric #-}

module SpatialMathT
       ( Rotation(..)
       , Rot(..)
       , V3T(..)
       , M33T
       , cross
       , orthonormalize
       ) where

import Control.Applicative ( Applicative )
import Data.Foldable ( Foldable )
import Data.Binary ( Binary(..) )
import Data.Serialize ( Serialize(..) )
import Data.Traversable ( Traversable )
import Foreign.Storable ( Storable )
import GHC.Generics ( Generic, Generic1 )

import Linear hiding ( cross )
import qualified Linear as L

import SpatialMath

newtype V3T f a = V3T {unV :: V3 a}
                deriving ( Functor, Foldable, Traversable
                         , Applicative
                         , Additive, Storable
                         , Num, Fractional, Eq, Show, Ord
                         , Generic1, Generic
                         , Serialize, Binary
                         )

cross :: Num a => V3T f a -> V3T f a -> V3T f a
cross (V3T vx) (V3T vy) = V3T (vx `L.cross` vy)

newtype Rot f1 f2 r =
  Rot { unR :: r }
  deriving ( Functor, Foldable, Traversable
           , Storable
           , Num, Fractional, Eq, Show, Ord
           , Generic1, Generic
           , Serialize, Binary
           )

type M33T f1 f2 a = V3T f1 (V3T f2 a)

class Rotation p a | p -> a where
  compose :: Rot f1 f2 p -> Rot f2 f3 p -> Rot f1 f3 p
  rot  :: Rot f1 f2 p -> V3T f1 a -> V3T f2 a
  rot' :: Rot f1 f2 p -> V3T f2 a -> V3T f1 a
  toDcm   :: Rot f1 f2 p -> Rot f1 f2 (M33 a)
--  fromDcm :: Rot f1 f2 (M33 a) -> Rot f1 f2 (p a)
  transpose :: Rot f1 f2 p -> Rot f2 f1 p

instance Num a => Rotation (Quaternion a) a where
  compose (Rot q_a2b) (Rot q_b2c) = Rot (q_a2b `quatMult` q_b2c)
  rot  (Rot q_a2b) (V3T va) = V3T (rotVecByQuat    q_a2b va)
  rot' (Rot q_a2b) (V3T vb) = V3T (rotVecByQuatB2A q_a2b vb)
  toDcm (Rot q_a2b) = Rot (dcmOfQuat q_a2b)
--  fromDcm (Rot dcm_a2b) = Rot (quatOfDcm dcm_a2b)
  transpose (Rot (Quaternion q0 qxyz)) = Rot (Quaternion q0 (fmap negate qxyz))

-- quaternion multiplication which doesn't require RealFrac
quatMult :: Num a => Quaternion a -> Quaternion a -> Quaternion a
quatMult (Quaternion s1 v1) (Quaternion s2 v2) =
  Quaternion (s1*s2 - (v1 `dot` v2)) $
  (v1 `L.cross` v2) + s1*^v2 + s2*^v1

instance Num a => Rotation (M33 a) a where
  compose (Rot dcm_a2b) (Rot dcm_b2c) = Rot (dcm_b2c !*! dcm_a2b)
  rot  (Rot dcm_a2b) (V3T va) = V3T (rotVecByDcm    dcm_a2b va)
  rot' (Rot dcm_a2b) (V3T vb) = V3T (rotVecByDcmB2A dcm_a2b vb)
  toDcm = id
  transpose (Rot (V3
                  (V3 e11 e12 e13)
                  (V3 e21 e22 e23)
                  (V3 e31 e32 e33))) =
    Rot (V3
         (V3 e11 e21 e31)
         (V3 e12 e22 e32)
         (V3 e13 e23 e33))

orthonormalize :: Floating a => Rot f1 f2 (M33 a) -> Rot f1 f2 (M33 a)
orthonormalize (Rot (V3
                     (V3 m00 m01 m02)
                     (V3 m10 m11 m12)
                     (V3 m20 m21 m22))) = Rot ret
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

    ret = (V3
           (V3 m00' m01'' m02'')
           (V3 m10' m11'' m12'')
           (V3 m20' m21'' m22''))
