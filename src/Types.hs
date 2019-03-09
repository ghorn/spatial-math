{-# OPTIONS_GHC -Wall #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}

module Types ( Euler(..) ) where

import GHC.Generics ( Generic, Generic1 )

import Control.Applicative ( Applicative(..) )
import Data.Data ( Data )
import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
import Data.Serialize ( Serialize )
import Data.Binary ( Binary )

-- | 3-2-1 Euler angle rotation sequence
data Euler a = Euler { eYaw :: a
                     , ePitch :: a
                     , eRoll :: a
                     } deriving ( Eq, Ord, Show, Functor, Foldable, Traversable
                                , Data, Generic, Generic1 )

instance Serialize a => Serialize (Euler a)
instance Binary a => Binary (Euler a)

instance Applicative Euler where
  pure x = Euler x x x
  Euler f0 f1 f2 <*> Euler x0 x1 x2 = Euler (f0 x0) (f1 x1) (f2 x2)
