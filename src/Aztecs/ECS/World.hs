{-# LANGUAGE AllowAmbiguousTypes #-}
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

import Aztecs.ECS.Access.Internal (Access)
import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.World.Bundle
import qualified Aztecs.ECS.World.Entities as E
import Aztecs.ECS.World.Internal (World (..))
import Data.Dynamic
import Data.IntMap (IntMap)
import Prelude hiding (lookup)

-- | Empty `World`.
empty :: World m
empty =
  World
    { entities = E.empty,
      nextEntityId = EntityID 0
    }

-- | Spawn a `Bundle` into the `World`. Returns the entity ID, updated world, and onInsert hook.
spawn :: (Monad m) => BundleT m -> World m -> (EntityID, World m, Access m ())
spawn b w =
  let e = nextEntityId w
      (es', hook) = E.spawn e b $ entities w
   in (e, w {entities = es', nextEntityId = EntityID $ unEntityId e + 1}, hook)

-- | Spawn an empty entity.
spawnEmpty :: World m -> (EntityID, World m)
spawnEmpty w = let e = nextEntityId w in (e, w {nextEntityId = EntityID $ unEntityId e + 1})

-- | Insert a `Bundle` into an entity. Returns updated world and onInsert hook.
insert :: (Monad m) => EntityID -> BundleT m -> World m -> (World m, Access m ())
insert e c w =
  let (es', hook) = E.insert e c (entities w)
   in (w {entities = es'}, hook)

-- | Lookup a component in an entity.
lookup :: forall m a. (Component m a) => EntityID -> World m -> Maybe a
lookup e w = E.lookup e $ entities w

-- | Remove a component from an entity. Returns the component (if found), updated world, and onRemove hook.
remove :: forall m a. (Component m a) => EntityID -> World m -> (Maybe a, World m, Access m ())
remove e w =
  let (a, es, hook) = E.remove e (entities w)
   in (a, w {entities = es}, hook)

-- | Remove a component from an entity with its `ComponentID`. Returns the component (if found), updated world, and onRemove hook.
removeWithId :: forall m a. (Component m a) => EntityID -> ComponentID -> World m -> (Maybe a, World m, Access m ())
removeWithId e cId w =
  let (a, es, hook) = E.removeWithId e cId (entities w)
   in (a, w {entities = es}, hook)

-- | Despawn an entity, returning its components.
despawn :: EntityID -> World m -> (IntMap Dynamic, World m)
despawn e w = let (a, es) = E.despawn e (entities w) in (a, w {entities = es})
