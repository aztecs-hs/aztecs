{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.World.Storage (Storage (..)) where

import Control.DeepSeq
import Data.Data

-- | Component storage, containing zero or many components of the same type.
class (Typeable s, NFData s, Typeable a) => Storage a s where
  -- | Storage with a single component.
  singleton :: a -> s

  -- | List of all components in the storage in ascending order.
  toAscList :: s -> [a]

  -- | Convert a sorted list of components (in ascending order) into a storage.
  fromAscList :: [a] -> s

instance (Typeable a, NFData a) => Storage a [a] where
  singleton a = [a]
  toAscList = id
  fromAscList = id
