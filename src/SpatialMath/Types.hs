{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveDataTypeable #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ScopedTypeVariables #-}

module SpatialMath.Types
  ( Dcm(..)
  , Euler(..)
  , Quat(..)
  , V3T(..), toV3, fromV3
  ) where

import GHC.Generics ( Generic, Generic1 )

import Codec.Serialise ( Serialise(..) )
import Control.Applicative ( Applicative(..) )
import Control.Lens ( Lens' )
import Data.Data ( Data )
import Data.Distributive ( Distributive(..) )
import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
import Data.Serialize ( Serialize )
import Data.Binary ( Binary )
import Foreign.Storable ( Storable )
import Linear ( Additive(..), Metric(..), Quaternion, R1(..), R2(..), R3(..), V3(..) )

-- | 3-2-1 Euler angle rotation sequence
data Euler f g a
  = Euler
    { eYaw :: a
    , ePitch :: a
    , eRoll :: a
    } deriving ( Eq, Ord, Show, Functor, Foldable, Traversable
               , Data, Generic, Generic1
               )
instance Serialize a => Serialize (Euler f g a)
instance Binary a => Binary (Euler f g a)
instance Serialise a => Serialise (Euler f g a)

instance Applicative (Euler f g) where
  pure x = Euler x x x
  Euler f0 f1 f2 <*> Euler x0 x1 x2 = Euler (f0 x0) (f1 x1) (f2 x2)


newtype V3T f a
  = V3T {unV :: V3 a}
  deriving ( Functor, Foldable, Traversable
           , Eq, Show, Ord
           , Generic, Generic1
           , Applicative, Additive
           , Binary, Serialize
           , Storable
           )

instance Metric (V3T f) where
  dot (V3T (V3 ax ay az)) (V3T (V3 bx by bz)) = ax * bx + ay * by + az * bz
  {-# INLINABLE dot #-}

instance Distributive (V3T f) where
  distribute f =
    V3T $ V3
    (fmap (\(V3T (V3 x _ _)) -> x) f)
    (fmap (\(V3T (V3 _ y _)) -> y) f)
    (fmap (\(V3T (V3 _ _ z)) -> z) f)
  {-# INLINE distribute #-}

instance Serialise a => Serialise (V3T f a) where
  encode (V3T (V3 x y z)) = encode x <> encode y <> encode z
  decode = do
    x <- decode
    y <- decode
    z <- decode
    pure (V3T (V3 x y z))

{-# INLINE v3TLens #-}
v3TLens :: Lens' (V3T f a) (V3 a)
v3TLens f = fmap fromV3 . f . toV3

toV3 :: V3T f a -> V3 a
toV3 = unV

fromV3 :: V3 a -> V3T f a
fromV3 = V3T

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

newtype Dcm f1 f2 a
  = DcmUnitVectors
    { getUnitVectors :: V3T f2 (V3T f1 a)
    } deriving ( Functor, Foldable, Traversable
               , Eq, Show, Ord
               , Generic1, Generic
               )
instance Serialize a => Serialize (Dcm f g a) where
instance Serialise a => Serialise (Dcm f g a) where
instance Binary a => Binary (Dcm f g a) where

newtype Quat f1 f2 a
  = Quat
    { unQuat :: Quaternion a
    } deriving ( Functor, Foldable, Traversable
               , Applicative
               , Storable
               , Eq, Show, Ord
               , Generic1, Generic
               , Binary, Serialize
               --, Serialise
               )
