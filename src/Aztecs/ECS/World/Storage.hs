{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.World.Storage (Storage (..)) where

import Control.DeepSeq
import Data.Data
import Prelude hiding (zipWith)
import qualified Prelude

-- | Component storage, containing zero or many components of the same type.
class (Typeable s, NFData s, Typeable a) => Storage a s where
  -- | Storage with a single component.
  singleton :: a -> s

  -- | List of all components in the storage in ascending order.
  toAscList :: s -> [a]

  -- | Convert a sorted list of components (in ascending order) into a storage.
  fromAscList :: [a] -> s

  map :: (a -> a) -> s -> s

  zipWith :: (i -> a -> a) -> [i] -> s -> ([a], s)

  zipWith_ :: (i -> a -> a) -> [i] -> s -> s
  zipWith_ f is as = snd $ zipWith f is as

instance (Typeable a, NFData a) => Storage a [a] where
  {-# INLINE singleton #-}
  singleton a = [a]
  {-# INLINE toAscList #-}
  toAscList = id
  {-# INLINE fromAscList #-}
  fromAscList = id
  {-# INLINE map #-}
  map = fmap
  {-# INLINE zipWith #-}
  zipWith f is as = let as' = Prelude.zipWith f is as in (as', as')
  {-# INLINE zipWith_ #-}
  zipWith_ = Prelude.zipWith
