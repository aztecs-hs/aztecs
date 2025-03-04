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

import Control.DeepSeq
import qualified Control.Monad
import Data.Data
import Prelude hiding (zipWith)
import qualified Prelude

-- | Component storage, containing zero or many components of the same type.
--
-- @since 9.0
class (Typeable s, NFData s, Typeable a) => Storage a s where
  -- | Storage with a single component.
  --
  -- @since 9.0
  singleton :: a -> s

  -- | List of all components in the storage in ascending order.
  --
  -- @since 9.0
  toAscList :: s -> [a]

  -- | Convert a sorted list of components (in ascending order) into a storage.
  --
  -- @since 9.0
  fromAscList :: [a] -> s

  -- | Map a function over all components in the storage.
  --
  --
  -- @since 9.0
  map :: (a -> a) -> s -> s

  -- | Map a function with some input over all components in the storage.
  --
  -- @since 9.0
  zipWith :: (i -> a -> a) -> [i] -> s -> ([a], s)

  -- | Map a monadic function with some input over all components in the storage.
  --
  -- @since 9.0
  zipWithM :: (Monad m) => (i -> a -> m a) -> [i] -> s -> m ([a], s)

  -- | Map a function with some input over all components in the storage.
  --
  -- @since 9.0
  zipWith_ :: (i -> a -> a) -> [i] -> s -> s
  zipWith_ f is as = snd $ zipWith f is as

-- | @since 9.0
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
  {-# INLINE zipWithM #-}
  zipWithM f is as = do
    as' <- Control.Monad.zipWithM f is as
    return (as', as')
