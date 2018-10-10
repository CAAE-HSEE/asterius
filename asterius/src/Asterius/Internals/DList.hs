{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Asterius.Internals.DList
  ( DList
  , fromList
  , toList
  , singleton
  ) where

import Data.Coerce
import Data.Monoid
import qualified GHC.Exts

newtype DList a =
  DList (Endo [a])
  deriving (Semigroup, Monoid)

instance GHC.Exts.IsList (DList a) where
  type Item (DList a) = a
  {-# INLINE fromList #-}
  fromList = fromList
  {-# INLINE toList #-}
  toList = toList

{-# INLINE fromList #-}
fromList :: [a] -> DList a
fromList = coerce . (<>)

{-# INLINE toList #-}
toList :: DList a -> [a]
toList = ($ []) . appEndo . coerce

{-# INLINE singleton #-}
singleton :: a -> DList a
singleton = coerce . (:)
