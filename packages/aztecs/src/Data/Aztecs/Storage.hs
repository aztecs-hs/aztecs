{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aztecs.Storage (Storage (..)) where

import Data.Data (Typeable)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- | Component storage, containing zero or many components of the same type.
class (Typeable (s a), Typeable a) => Storage s a where
  -- | Storage with a single component.
  singleton :: Int -> a -> s a

  -- | All components in the storage.
  all :: s a -> [(Int, a)]

  -- | Insert a component into the storage.
  insert :: Int -> a -> s a -> s a

  -- | Lookup a component in the storage.
  lookup :: Int -> s a -> Maybe a

  -- | Convert a sorted list of components (in ascending order) into a storage.
  fromAscList :: [(Int, a)] -> s a

  -- | Remove a component from the storage.
  remove :: Int -> s a -> (Maybe a, s a)

instance (Typeable a) => Storage IntMap a where
  singleton = IntMap.singleton
  all = IntMap.toList
  insert = IntMap.insert
  lookup = IntMap.lookup
  fromAscList = IntMap.fromAscList
  remove i s = (IntMap.lookup i s, IntMap.delete i s) -- TODO remove double lookup
