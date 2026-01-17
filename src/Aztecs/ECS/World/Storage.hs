{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Aztecs.ECS.World.Storage
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Storage (Storage (..)) where

import Data.Data
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (map, zipWith)

-- | Component storage, containing zero or many components of the same type.
--
-- @since 0.9
class (Typeable s, Typeable a) => Storage a s where
  -- | Storage with a single component.
  --
  -- @since 0.9
  singleton :: a -> s

  -- | Vector of all components in the storage in ascending order.
  --
  -- @since 0.9
  toAscVector :: s -> Vector a

  -- | Convert a sorted vector of components (in ascending order) into a storage.
  --
  -- @since 0.9
  fromAscVector :: Vector a -> s

  -- | Map a function over all components in the storage.
  --
  --
  -- @since 0.9
  map :: (a -> a) -> s -> s

  -- | Map a function with some input over all components in the storage.
  --
  -- @since 0.9
  zipWith :: (i -> a -> a) -> Vector i -> s -> (Vector a, s)

  -- | Map an applicative functor with some input over all components in the storage.
  --
  -- @since 0.9
  zipWithM :: (Monad m) => (i -> a -> m a) -> Vector i -> s -> m (Vector a, s)

  -- | Map a function with some input over all components in the storage.
  --
  -- @since 0.9
  zipWith_ :: (i -> a -> a) -> Vector i -> s -> s
  zipWith_ f is as = snd $ zipWith f is as

-- | @since 0.9
instance (Typeable a) => Storage a (Vector a) where
  singleton a = V.singleton a
  {-# INLINE singleton #-}

  toAscVector = id
  {-# INLINE toAscVector #-}

  fromAscVector = id
  {-# INLINE fromAscVector #-}

  map = V.map
  {-# INLINE map #-}

  zipWith f is as = let as' = V.zipWith f is as in (as', as')
  {-# INLINE zipWith #-}

  zipWith_ f is as = V.zipWith f is as
  {-# INLINE zipWith_ #-}

  zipWithM f is as = (\as' -> (as', as')) <$> V.zipWithM f is as
  {-# INLINE zipWithM #-}
