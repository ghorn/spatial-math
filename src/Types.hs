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

import Data.Data ( Data )
import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
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

