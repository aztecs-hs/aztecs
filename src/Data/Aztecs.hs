{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs where

import Data.Data (Proxy (..), TypeRep, Typeable)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Typeable (typeOf)

class (Typeable (s a)) => Storage s a where
  singleton :: Int -> a -> s a
  all :: s a -> [(Int, a)]
  insert :: Int -> a -> s a -> s a

instance (Typeable a) => Storage IntMap a where
  singleton = IntMap.singleton
  all = IntMap.toList
  insert = IntMap.insert

newtype EntityID = EntityID {unEntityId :: Int}

class (Typeable a, Storage (StorageT a) a) => Component a where
  type StorageT a :: Type -> Type
  type StorageT a = IntMap

data World = World
  { storages :: Map TypeRep Dynamic,
    nextEntityId :: EntityID
  }

world :: World
world =
  World
    { storages = Map.empty,
      nextEntityId = EntityID 0
    }

spawn :: forall a. (Component a) => a -> World -> (EntityID, World)
spawn c w =
  let e = nextEntityId w
      storage = case Map.lookup (typeOf (Proxy @a)) (storages w) of
        Just s -> insert (unEntityId e) c (fromMaybe (error "TODO") (fromDynamic s))
        Nothing -> singleton @(StorageT a) @a (unEntityId e) c
   in ( e,
        w
          { storages = Map.insert (typeOf (Proxy @a)) (toDyn storage) (storages w),
            nextEntityId = EntityID (unEntityId e + 1)
          }
      )
