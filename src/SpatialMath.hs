{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language InstanceSigs #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

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

    -- conversions
  , dcmOfQuat
  , dcmOfEuler321
  , quatOfDcm
  , quatOfEuler321
  , euler321OfDcm
  , unsafeEuler321OfDcm
  , euler321OfQuat
  , unsafeEuler321OfQuat

    -- rotations
  , Rotate(..)
  , transposeDcm, composeDcm
  , qmult, qtranspose

  , identity, qidentity

    -- points
  , type (-->)
  , Plus
  , Minus
  , V3P(..), definePoint
  , (.+.), (.-.), negateV
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


class Rotate g where
  rot :: Num a => Dcm f1 f2 a -> g f1 a -> g f2 a

instance Rotate V3T where
  rot :: Num a => Dcm f1 f2 a -> V3T f1 a -> V3T f2 a
  rot (DcmUnitVectors dcm) vec = dcm !* vec

instance Rotate (V3P v) where
  rot :: Num a => Dcm f g a -> V3P v f a -> V3P v g a
  rot dcm (UnsafeV3P v) = UnsafeV3P (rot dcm v)


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


-- | Vector @v@ represented with bases @f@.
newtype V3P v f a = UnsafeV3P {unsafeUnV3P :: V3T f a}

-- | Origin of a frame $f$.
data Origin f

-- | Define point with respect to origin.
definePoint :: V3T f a -> V3P (Origin f --> p) f a
definePoint = UnsafeV3P

-- | Vector from point to point.
data f --> g
infixl 8 --> -- TODO(greg): what the heck should this fixity be?

-- | Vector addition
data v1 `Plus` v2
infixl 6 `Plus`

-- | Vector subtraction.
--data v2 `Minus` v1
-- This isn't really needed, you can just define it as flip (-->):
type Minus v2 v1 = v1 --> v2
infixl 6 `Minus`

-- | Type family for basic vector arithmetic simplification
type family Simplify a where
  -- (a -> b) + (b -> c) == a -> c
  Simplify ((a --> b) `Plus` (b --> c)) = Simplify (a --> c)
  -- (b -> c) + (a -> b) == a -> c
  Simplify ((b --> c) `Plus` (a --> b)) = Simplify (a --> c)

--  -- (a -> c) - (b -> c) == (a -> b)
--  Simplify ((a --> c) `Minus` (b --> c)) = Simplify (a --> b)  -- redundant if Minus is a type alias
--  -- (a -> c) - (a -> b) == (b -> c)
--  Simplify ((a --> c) `Minus` (a --> b)) = Simplify (b --> c)  -- redundant if Minus is a type alias

  Simplify ((a `Plus` b) `Minus` a) = Simplify b
  Simplify ((a `Minus` b) `Plus` b) = Simplify a
--  Simplify (a --> (a `Plus` b)) = Simplify b  -- redundant if Minus is a type alias

  -- (a -> c) - (b -> c) == (a -> b)
  Simplify ((b --> c) --> (a --> c)) = Simplify (a --> b)
  -- (a -> c) - (a -> b) == (b -> c)
  Simplify ((a --> b) --> (a --> c)) = Simplify (b --> c)

  -- should we support unit?
  Simplify (a --> a) = ()
  Simplify (a `Plus` ()) = Simplify a
  Simplify (a `Minus` ()) = Simplify a
  Simplify (() `Plus` a) = Simplify a
  Simplify (() `Minus` (a --> b)) = Simplify (b --> a)

  -- If no simplifications can be made at the top level, recurse.
  -- It would be nice to iterate here, so if a simplification was made it would
  -- start the patterm matching over, but if no simplification was made then it would stop.
  -- I don't know how to write that.
  -- Maybe type level tics, like
  --
  --   type family Simplify n a where
  --     Simplify 0 (a --> b) = (Simplify a) --> (Simplify b)
  --     Simplify n (a --> b) = Simplify (n - 1) ((Simplify a) --> (Simplify b))
  --
  Simplify (a --> b) = (Simplify a) --> (Simplify b)
  Simplify (a `Plus` b) = (Simplify a) `Plus` (Simplify b)
  --Simplify (a `Minus` b) = (Simplify a) `Minus` (Simplify b) -- redundant if Minus is a type alias

  -- base case where nothing more can be done
  Simplify a = a

-- | vector addition
infixl 6 .+.
(.+.) :: Num a => V3P v1 f a -> V3P v2 f a -> V3P (Simplify (v1 `Plus` v2)) f a
(.+.) (UnsafeV3P x) (UnsafeV3P y) = UnsafeV3P (x ^+^ y)

-- | vector subtraction
infixl 6 .-.
(.-.) :: Num a => V3P v2 f a -> V3P v1 f a -> V3P (Simplify (v1 --> v2)) f a
--(.-.) :: Num a => V3P v2 f a -> V3P v1 f a -> V3P (Simplify (v2 `Minus` v1)) f a
(.-.) (UnsafeV3P y) (UnsafeV3P x) = UnsafeV3P (y ^-^ x)

-- | vector negation
negateV :: Num a => V3P (a --> b) f a -> V3P (b --> a) f a
negateV (UnsafeV3P x) = UnsafeV3P (negate <$> x)


--------- The rest of this file is a test of vector composition/subtraction ---------
data A
data B
data C

data F -- frame

-- origin convenience alias
-- type V3O p0 p1 f = V3P (Origin f) p1 f

------------ define three points in frame F -------
_va :: V3P (Origin F --> A) F Double
_va = definePoint (V3T (V3 1 2 3))

_vb :: V3P (Origin F --> B) F Double
_vb = definePoint (V3T (V3 4 5 6))

_vc :: V3P (Origin F --> C) F Double
_vc = definePoint (V3T (V3 7 8 9))

--------- difference them and make sure types line up ----------
-- vector from b to c
_vbc :: V3P (B --> C) F Double
_vbc = _vc .-. _vb

-- vector from a to b
_vab :: V3P (A --> B) F Double
_vab = _vb .-. _va

--------- compose differences and make sure types line up ----------
-- vector from a to c
_vac :: V3P (A --> C) F Double
_vac = _vab .+. _vbc

-- vector from origin to c
_vc' :: V3P (Origin F --> C) F Double
_vc' = (_va .+. _vc) .-. _va

_vc'' :: V3P (Origin F --> C) F Double
_vc'' = (_vc .-. _va) .+. _va

_vc''' :: V3P (Origin F --> C) F Double
_vc''' = _va .+. (_vc .-. _va)
