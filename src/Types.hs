{-# OPTIONS_GHC -Wall #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# Language DeriveGeneric #-}
#endif
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}

module Types ( Euler(..) ) where

import Control.Applicative ( Applicative(..) )
import Data.Data ( Data )
import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
import GHC.Generics ( Generic )
import Data.Serialize ( Serialize )
import Data.Binary ( Binary )
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
import GHC.Generics ( Generic1 )
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
import Data.Typeable ( Typeable )
#else
import Data.Typeable ( Typeable1 )
#endif

-- | 3-2-1 Euler angle rotation sequence
data Euler a = Euler { eYaw :: a
                     , ePitch :: a
                     , eRoll :: a
                     } deriving (Eq, Show, Functor, Foldable, Traversable, Ord
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
                                , Generic
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
                                , Generic1
#endif
                                )

deriving instance Data a => Data (Euler a)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
deriving instance Typeable Euler
#else
deriving instance Typeable1 Euler
#endif


#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
instance Serialize a => Serialize (Euler a)
instance Binary a => Binary (Euler a)
#endif

instance Applicative Euler where
  pure x = Euler x x x
  Euler f0 f1 f2 <*> Euler x0 x1 x2 = Euler (f0 x0) (f1 x1) (f2 x2)
