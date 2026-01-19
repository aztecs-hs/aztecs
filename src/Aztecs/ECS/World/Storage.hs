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
class (Typeable s, Typeable a) => Storage a s where
  -- | Storage with a single component.
  singleton :: a -> s

  -- | Vector of all components in the storage in ascending order.
  toAscVector :: s -> Vector a

  -- | Convert a sorted vector of components (in ascending order) into a storage.
  fromAscVector :: Vector a -> s

  -- | Map a function over all components in the storage.
  map :: (a -> a) -> s -> s

  -- | Map a function with some input over all components in the storage.
  zipWith :: (i -> a -> a) -> Vector i -> s -> (Vector a, s)

  -- | Map an applicative functor with some input over all components in the storage.
  zipWithM :: (Monad m) => (i -> a -> m a) -> Vector i -> s -> m (Vector a, s)

  -- | Map a function with some input over all components in the storage.
  zipWith_ :: (i -> a -> a) -> Vector i -> s -> s
  zipWith_ f is as = snd $ zipWith f is as

  -- | Map a function with some input over all components, returning a tuple result and updated storage.
  zipWithAccum :: (i -> a -> (o, a)) -> Vector i -> s -> (Vector (o, a), s)

  -- | Map a monadic function with some input over all components, returning a tuple result and updated storage.
  zipWithAccumM :: (Monad m) => (i -> a -> m (o, a)) -> Vector i -> s -> m (Vector (o, a), s)

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

  zipWithAccum f is as =
    let pairs = V.zipWith f is as
        as' = V.map snd pairs
     in (pairs, as')
  {-# INLINE zipWithAccum #-}

  zipWithAccumM f is as = do
    pairs <- V.zipWithM f is as
    let as' = V.map snd pairs
    return (pairs, as')
  {-# INLINE zipWithAccumM #-}
