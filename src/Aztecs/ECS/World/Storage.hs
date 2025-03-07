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
-- @since 0.9
class (Typeable s, NFData s, Typeable a) => Storage a s where
  -- | Storage with a single component.
  --
  -- @since 0.9
  singleton :: a -> s

  -- | List of all components in the storage in ascending order.
  --
  -- @since 0.9
  toAscList :: s -> [a]

  -- | Convert a sorted list of components (in ascending order) into a storage.
  --
  -- @since 0.9
  fromAscList :: [a] -> s

  -- | Map a function over all components in the storage.
  --
  --
  -- @since 0.9
  map :: (a -> a) -> s -> s

  -- | Map a function with some input over all components in the storage.
  --
  -- @since 0.9
  zipWith :: (b -> a -> (c, a)) -> [b] -> s -> ([(c, a)], s)

  -- | Map an applicative functor with some input over all components in the storage.
  --
  -- @since 0.9
  zipWithM :: (Applicative m) => (b -> a -> m (c, a)) -> [b] -> s -> m ([(c, a)], s)

-- | @since 0.9
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
  zipWith f is as = let as' = Prelude.zipWith f is as in (as', fmap snd as')
  {-# INLINE zipWithM #-}
  zipWithM f is as = (\as' -> (as', fmap snd as')) <$> Control.Monad.zipWithM f is as
