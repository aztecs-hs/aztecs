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

import Control.Monad (zipWithM)
import Data.Data
import qualified Data.List as L
import Prelude hiding (map, zipWith)

-- | Component storage, containing zero or many components of the same type.
class (Typeable s, Typeable a) => Storage a s where
  -- | Storage with a single component.
  singleton :: a -> s

  -- | List of all components in the storage in ascending order.
  toAsc :: s -> [a]

  -- | Convert a sorted list of components (in ascending order) into a storage.
  fromAsc :: [a] -> s

  -- | Map a function over all components in the storage.
  map :: (a -> a) -> s -> s

  -- | Map a function with some input over all components in the storage.
  zipWith :: (i -> a -> a) -> [i] -> s -> ([a], s)

  -- | Map an applicative functor with some input over all components in the storage.
  zipWithM :: (Monad m) => (i -> a -> m a) -> [i] -> s -> m ([a], s)

  -- | Map a function with some input over all components in the storage.
  zipWith_ :: (i -> a -> a) -> [i] -> s -> s
  zipWith_ f is as = snd $ zipWith f is as

  -- | Map a function with some input over all components, returning a tuple result and updated storage.
  zipWithAccum :: (i -> a -> (o, a)) -> [i] -> s -> ([(o, a)], s)

  -- | Map a monadic function with some input over all components, returning a tuple result and updated storage.
  zipWithAccumM :: (Monad m) => (i -> a -> m (o, a)) -> [i] -> s -> m ([(o, a)], s)

instance (Typeable a) => Storage a [a] where
  singleton a = [a]
  {-# INLINE singleton #-}

  toAsc = id
  {-# INLINE toAsc #-}

  fromAsc = id
  {-# INLINE fromAsc #-}

  map = fmap
  {-# INLINE map #-}

  zipWith f is as = let as' = L.zipWith f is as in (as', as')
  {-# INLINE zipWith #-}

  zipWith_ f is as = L.zipWith f is as
  {-# INLINE zipWith_ #-}

  zipWithM f is as = (\as' -> (as', as')) <$> Control.Monad.zipWithM f is as
  {-# INLINE zipWithM #-}

  zipWithAccum f is as =
    let pairs = L.zipWith f is as
        as' = fmap snd pairs
     in (pairs, as')
  {-# INLINE zipWithAccum #-}

  zipWithAccumM f is as = do
    pairs <- Control.Monad.zipWithM f is as
    let as' = fmap snd pairs
    return (pairs, as')
  {-# INLINE zipWithAccumM #-}
