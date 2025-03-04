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
import Control.DeepSeq
import Data.Dynamic
import Data.IntMap (IntMap)
import GHC.Generics
import Prelude hiding (lookup)

-- | World of entities and their components.
--
-- @since 0.9
data World = World
  { -- | Entities and their components.
    --
    -- @since 0.9
    entities :: !Entities,
    -- | Next unique entity identifier.
    --
    -- @since 0.9
    nextEntityId :: !EntityID
  }
  deriving (Show, Generic, NFData)

-- | Empty `World`.
--
-- @since 0.9
empty :: World
empty =
  World
    { entities = E.empty,
      nextEntityId = EntityID 0
    }

-- | Spawn a `Bundle` into the `World`.
--
-- @since 0.9
spawn :: Bundle -> World -> (EntityID, World)
spawn b w =
  let e = nextEntityId w
   in (e, w {entities = E.spawn e b $ entities w, nextEntityId = EntityID $ unEntityId e + 1})

-- | Spawn an empty entity.
--
-- @since 0.9
spawnEmpty :: World -> (EntityID, World)
spawnEmpty w = let e = nextEntityId w in (e, w {nextEntityId = EntityID $ unEntityId e + 1})

-- | Insert a `Bundle` into an entity.
--
-- @since 0.9
insert :: EntityID -> Bundle -> World -> World
insert e c w = w {entities = E.insert e c (entities w)}

-- | Lookup a component in an entity.
--
-- @since 0.9
lookup :: forall a. (Component a) => EntityID -> World -> Maybe a
lookup e w = E.lookup e $ entities w

-- | Remove a component from an entity.
--
-- @since 0.9
remove :: forall a. (Component a) => EntityID -> World -> (Maybe a, World)
remove e w = let (a, es) = E.remove e (entities w) in (a, w {entities = es})

-- | Remove a component from an entity with its `ComponentID`.
--
-- @since 0.9
removeWithId :: forall a. (Component a) => EntityID -> ComponentID -> World -> (Maybe a, World)
removeWithId e cId w = let (a, es) = E.removeWithId e cId (entities w) in (a, w {entities = es})

-- | Despawn an entity, returning its components.
despawn :: EntityID -> World -> (IntMap Dynamic, World)
despawn e w = let (a, es) = E.despawn e (entities w) in (a, w {entities = es})
