{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
data World = World
  { entities :: !Entities,
    nextEntityId :: !EntityID
  }
  deriving (Show, Generic, NFData)

-- | Empty `World`.
empty :: World
empty =
  World
    { entities = E.empty,
      nextEntityId = EntityID 0
    }

spawn :: Bundle -> World -> (EntityID, World)
spawn b w =
  let e = nextEntityId w
   in (e, w {entities = E.spawn e b $ entities w, nextEntityId = EntityID $ unEntityId e + 1})

-- | Spawn an empty entity.
spawnEmpty :: World -> (EntityID, World)
spawnEmpty w = let e = nextEntityId w in (e, w {nextEntityId = EntityID $ unEntityId e + 1})

-- | Insert a `Bundle` into an entity.
insert :: EntityID -> Bundle -> World -> World
insert e c w = w {entities = E.insert e c (entities w)}

lookup :: forall a. (Component a) => EntityID -> World -> Maybe a
lookup e w = E.lookup e $ entities w

-- | Insert a component into an entity.
remove :: forall a. (Component a) => EntityID -> World -> (Maybe a, World)
remove e w = let (a, es) = E.remove e (entities w) in (a, w {entities = es})

removeWithId :: forall a. (Component a) => EntityID -> ComponentID -> World -> (Maybe a, World)
removeWithId e cId w = let (a, es) = E.removeWithId e cId (entities w) in (a, w {entities = es})

-- | Despawn an entity, returning its components.
despawn :: EntityID -> World -> (IntMap Dynamic, World)
despawn e w = let (a, es) = E.despawn e (entities w) in (a, w {entities = es})
