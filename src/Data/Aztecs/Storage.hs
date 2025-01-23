{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aztecs.Storage where

import Data.Data (Typeable)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

class (Typeable (s a)) => Storage s a where
  singleton :: Int -> a -> s a
  all :: s a -> [(Int, a)]
  insert :: Int -> a -> s a -> s a
  lookup :: Int -> s a -> Maybe a
  mapComponents :: (a -> a) -> s a -> s a
  mapComponentsFilter :: [Int] -> (a -> a) -> s a -> ([(Int, a)], s a)

instance (Typeable a) => Storage IntMap a where
  singleton = IntMap.singleton
  all = IntMap.toList
  insert = IntMap.insert
  lookup = IntMap.lookup
  mapComponents = IntMap.map
  mapComponentsFilter es f s =
    let s' = IntMap.mapWithKey (\e c -> if e `elem` es then f c else c) s
     in (IntMap.toList s', s')
