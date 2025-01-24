{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aztecs.Storage (Storage(..)) where

import Data.Data (Typeable)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

class (Typeable (s a), Typeable a) => Storage s a where
  singleton :: Int -> a -> s a
  all :: s a -> [(Int, a)]
  insert :: Int -> a -> s a -> s a
  lookup :: Int -> s a -> Maybe a
  fromAscList :: [(Int, a)] -> s a
  remove :: Int -> s a -> (Maybe a, s a)

instance (Typeable a) => Storage IntMap a where
  singleton = IntMap.singleton
  all = IntMap.toList
  insert = IntMap.insert
  lookup = IntMap.lookup
  fromAscList = IntMap.fromAscList
  remove i s = (IntMap.lookup i s, IntMap.delete i s) -- TODO remove double lookup
