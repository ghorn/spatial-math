{-# OPTIONS_GHC -Wall #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveGeneric #-}
{-# Language TypeOperators #-}

module SpatialMath
  ( ArcTan2(..)
  , Euler(..)
  , Quaternion(..), V3(..)
  , Rotation(..)
  , Rot(..)
  , V3T(..)
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
    -- * re-export for convenience
  , Compose(..)
  ) where

import GHC.Generics ( Generic, Generic1 )

import Codec.Serialise ( Serialise(..) )
import Control.Lens ( Lens' )
import Data.Binary ( Binary(..) )
import Data.Serialize ( Serialize(..) )
import Foreign.Storable ( Storable )
import Linear hiding ( cross, normalize, transpose )
import qualified Linear as L
import Data.Functor.Compose

import SpatialMath.ArcTan2 ( ArcTan2(..) )
import SpatialMath.Euler ( Euler(..) )
import qualified SpatialMath.Untyped as SM


newtype V3T f a = V3T {unV :: V3 a}
                deriving ( Functor, Foldable, Traversable
                         , Applicative
                         , Additive, Metric, Storable
                         , Num, Fractional, Eq, Show, Ord
                         , Generic1, Generic
                         , Serialize, Binary
                         -- TODO(greg): add Serialise after Linear adds it.
                         )

{-# INLINE v3TLens #-}
v3TLens :: Lens' (V3T f a) (V3 a)
v3TLens f = fmap V3T . f . unV

instance R1 (V3T f) where
  _x = v3TLens . _x
  {-# INLINE _x #-}
instance R2 (V3T f) where
  _y = v3TLens . _y
  {-# INLINE _y #-}
  _xy = v3TLens . _xy
  {-# INLINE _xy #-}
instance R3 (V3T f) where
  _z = v3TLens . _z
  {-# INLINE _z #-}
  _xyz = v3TLens . _xyz
  {-# INLINE _xyz #-}

cross :: Num a => V3T f a -> V3T f a -> V3T f a
cross (V3T vx) (V3T vy) = V3T (vx `L.cross` vy)

newtype Rot f1 f2 r a =
  Rot { unRot :: r a }
  deriving ( Functor, Foldable, Traversable
           , Applicative
           , Storable
           , Num, Fractional, Eq, Show, Ord
           , Generic1, Generic
           , Binary, Serialize, Serialise
           )

class Rotation g a where
  compose :: Rot f1 f2 g a -> Rot f2 f3 g a -> Rot f1 f3 g a
  rot  :: Rot f1 f2 g a -> V3T f1 a -> V3T f2 a
  rot' :: Rot f1 f2 g a -> V3T f2 a -> V3T f1 a
  transpose :: Rot f1 f2 g a -> Rot f2 f1 g a
  identity :: Rot f1 f2 g a

instance Num a => Rotation Quaternion a where
  compose (Rot q_a2b) (Rot q_b2c) = Rot (q_a2b `quatMult` q_b2c)
    where
      -- quaternion multiplication which doesn't require RealFrac
      quatMult :: Num a => Quaternion a -> Quaternion a -> Quaternion a
      quatMult (Quaternion s1 v1) (Quaternion s2 v2) =
        Quaternion (s1*s2 - (v1 `dot` v2)) $
        (v1 `L.cross` v2) + s1*^v2 + s2*^v1

  rot  (Rot q_a2b) (V3T va) = V3T (SM.rotVecByQuat    q_a2b va)
  rot' (Rot q_a2b) (V3T vb) = V3T (SM.rotVecByQuatB2A q_a2b vb)
  transpose (Rot (Quaternion q0 qxyz)) = Rot (Quaternion q0 (fmap negate qxyz))
  identity = Rot (Quaternion 1 (pure 0))

instance Num a => Rotation (Compose V3 V3) a where
  compose (Rot (Compose dcm_a2b)) (Rot (Compose dcm_b2c)) = Rot $ Compose (dcm_b2c !*! dcm_a2b)
  rot  (Rot (Compose dcm_a2b)) (V3T va) = V3T (SM.rotVecByDcm    dcm_a2b va)
  rot' (Rot (Compose dcm_a2b)) (V3T vb) = V3T (SM.rotVecByDcmB2A dcm_a2b vb)
  transpose
    (Rot
     (Compose
      (V3
       (V3 e11 e12 e13)
       (V3 e21 e22 e23)
       (V3 e31 e32 e33)))) =
    Rot $ Compose $
    V3
    (V3 e11 e21 e31)
    (V3 e12 e22 e32)
    (V3 e13 e23 e33)
  identity =
    Rot $ Compose $
    V3
    (V3 1 0 0)
    (V3 0 1 0)
    (V3 0 0 1)


dcmOfQuat :: Num a => Rot f g Quaternion a -> Rot f g (Compose V3 V3) a
dcmOfQuat = Rot . Compose . SM.dcmOfQuat . unRot

dcmOfEuler321 :: Floating a => Rot f g Euler a -> Rot f g (Compose V3 V3) a
dcmOfEuler321 = Rot . Compose . SM.dcmOfEuler321 . unRot

quatOfDcm :: (Floating a, Ord a) => Rot f g (Compose V3 V3) a -> Rot f g Quaternion a
quatOfDcm = Rot . SM.quatOfDcm . getCompose . unRot

quatOfEuler321 :: Floating a => Rot f g Euler a -> Rot f g Quaternion a
quatOfEuler321 = Rot . SM.quatOfEuler321 . unRot

unsafeEuler321OfDcm :: ArcTan2 a => Rot f g (Compose V3 V3) a -> Rot f g Euler a
unsafeEuler321OfDcm = Rot . SM.unsafeEuler321OfDcm . getCompose . unRot

euler321OfDcm :: (ArcTan2 a, Ord a) => Rot f g (Compose V3 V3) a -> Rot f g Euler a
euler321OfDcm = Rot . SM.euler321OfDcm . getCompose . unRot

euler321OfQuat :: (ArcTan2 a, Ord a) => Rot f g Quaternion a -> Rot f g Euler a
euler321OfQuat = Rot . SM.euler321OfQuat . unRot

unsafeEuler321OfQuat :: ArcTan2 a => Rot f g Quaternion a -> Rot f g Euler a
unsafeEuler321OfQuat = Rot . SM.unsafeEuler321OfQuat . unRot

instance (ArcTan2 a, Floating a, Ord a) => Rotation Euler a where
  -- defined in terms of quaternion composition
  compose e_a2b e_b2c = euler321OfQuat q_a2c
    where
      q_a2b = quatOfEuler321 e_a2b
      q_b2c = quatOfEuler321 e_b2c
      q_a2c = compose q_a2b q_b2c

  rot  (Rot e_a2b) (V3T va) = V3T (SM.rotVecByEuler e_a2b va)
  rot' (Rot e_a2b) (V3T vb) = V3T (SM.rotVecByEulerB2A e_a2b vb)
  transpose = euler321OfQuat . transpose . quatOfEuler321
  identity = Rot (Euler 0 0 0)

orthonormalize :: Floating a => Rot f1 f2 (Compose V3 V3) a -> Rot f1 f2 (Compose V3 V3) a
orthonormalize
  (Rot
   (Compose
    (V3
     (V3 m00 m01 m02)
     (V3 m10 m11 m12)
     (V3 m20 m21 m22)))) = Rot (Compose ret)
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
