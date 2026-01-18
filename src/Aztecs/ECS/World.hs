{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.ECS.World
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World
  ( World (..),
    empty,
    spawn,
    spawnEmpty,
    insert,
    lookup,
    remove,
    removeWithId,
    despawn,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.World.Bundle
import Aztecs.ECS.World.Entities (Entities)
import qualified Aztecs.ECS.World.Entities as E
import Data.Dynamic
import Data.IntMap (IntMap)
import GHC.Generics
import Prelude hiding (lookup)

-- | World of entities and their components.
data World m = World
  { -- | Entities and their components.
    entities :: !(Entities m),
    -- | Next unique entity identifier.
    nextEntityId :: !EntityID
  }
  deriving (Show, Generic)

-- | Empty `World`.
empty :: World m
empty =
  World
    { entities = E.empty,
      nextEntityId = EntityID 0
    }

-- | Spawn a `Bundle` into the `World`.
spawn :: (Monad m) => BundleT m -> World m -> m (EntityID, World m)
spawn b w = do
  let e = nextEntityId w
  es' <- E.spawn e b $ entities w
  return (e, w {entities = es', nextEntityId = EntityID $ unEntityId e + 1})

-- | Spawn an empty entity.
spawnEmpty :: World m -> (EntityID, World m)
spawnEmpty w = let e = nextEntityId w in (e, w {nextEntityId = EntityID $ unEntityId e + 1})

-- | Insert a `Bundle` into an entity.
insert :: (Monad m) => EntityID -> BundleT m -> World m -> m (World m)
insert e c w = do
  es' <- E.insert e c (entities w)
  return w {entities = es'}

-- | Lookup a component in an entity.
lookup :: forall m a. (Component m a) => EntityID -> World m -> Maybe a
lookup e w = E.lookup e $ entities w

-- | Remove a component from an entity.
remove :: forall m a. (Component m a) => EntityID -> World m -> m (Maybe a, World m)
remove e w = do
  (a, es) <- E.remove e (entities w)
  return (a, w {entities = es})

-- | Remove a component from an entity with its `ComponentID`.
removeWithId :: forall m a. (Component m a) => EntityID -> ComponentID -> World m -> m (Maybe a, World m)
removeWithId e cId w = do
  (a, es) <- E.removeWithId e cId (entities w)
  return (a, w {entities = es})

-- | Despawn an entity, returning its components.
despawn :: EntityID -> World m -> (IntMap Dynamic, World m)
despawn e w = let (a, es) = E.despawn e (entities w) in (a, w {entities = es})
