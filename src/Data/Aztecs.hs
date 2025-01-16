{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs where

import Data.Aztecs.Storage (Storage)
import qualified Data.Aztecs.Storage as S
import Data.Data (Proxy (..), TypeRep, Typeable)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Typeable (typeOf)
import Prelude hiding (all, lookup)

newtype EntityID = EntityID {unEntityId :: Int}
  deriving (Show)

class (Typeable a, Storage (StorageT a) a) => Component a where
  type StorageT a :: Type -> Type
  type StorageT a = IntMap

data World = World
  { storages :: Map TypeRep Dynamic,
    nextEntityId :: EntityID
  }
  deriving (Show)

world :: World
world =
  World
    { storages = Map.empty,
      nextEntityId = EntityID 0
    }

spawn :: forall a. (Component a) => a -> World -> (EntityID, World)
spawn c w =
  let e = nextEntityId w
   in (e, insert e c w {nextEntityId = EntityID (unEntityId e + 1)})

insert :: forall a. (Component a) => EntityID -> a -> World -> World
insert e c w =
  let storage = case Map.lookup (typeOf (Proxy @a)) (storages w) of
        Just s -> S.insert (unEntityId e) c (fromMaybe (error "TODO") (fromDynamic s))
        Nothing -> S.singleton @(StorageT a) @a (unEntityId e) c
   in w {storages = Map.insert (typeOf (Proxy @a)) (toDyn storage) (storages w)}

all :: forall a. (Component a) => World -> [(EntityID, a)]
all w = fromMaybe [] $ do
  dynS <- Map.lookup (typeOf (Proxy @a)) (storages w)
  s <- fromDynamic @((StorageT a) a) dynS
  return . map (\(i, c) -> (EntityID i, c)) $ S.all s

lookup :: forall a. (Component a) => EntityID -> World -> Maybe a
lookup e w = do
  dynS <- Map.lookup (typeOf (Proxy @a)) (storages w)
  s <- fromDynamic @((StorageT a) a) dynS
  S.lookup (unEntityId e) s
