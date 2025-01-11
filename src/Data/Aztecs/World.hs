{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.World
  ( ComponentID (..),
    World (..),
    empty,
    spawn,
    spawnWithId,
    spawnDyn,
    insert,
    lookup,
    lookupDyn,
  )
where

import Data.Aztecs
import Data.Aztecs.Archetypes (Archetypes)
import qualified Data.Aztecs.Archetypes as AS
import Data.Aztecs.Components (ComponentID (..), Components)
import qualified Data.Aztecs.Components as CS
import Data.Data (Typeable)
import Data.Dynamic (Dynamic, toDyn)
import Prelude hiding (lookup)

-- | World of entities and components.
data World = World
  { archetypes :: Archetypes,
    components :: Components,
    nextEntityID:: EntityID
  }
  deriving (Show)

-- | Empty world.
empty :: World
empty =
  World
    { archetypes = AS.empty,
      components = CS.empty,
      nextEntityID= EntityID 0
    }

-- | Spawn an entity with a component.
spawn :: forall c. (Typeable c) => c -> World -> (EntityID, World)
spawn c w = case CS.lookup @c (components w) of
  Just cId -> spawnWithId cId c w
  Nothing ->
    let (cId, cs) = CS.insert @c (components w)
        w' = w {components = cs}
        e = nextEntityID w'
     in ( e,
          ( w'
              { archetypes = AS.insertNewComponent e cId c (archetypes w),
                nextEntityID= EntityID(unEntityID e + 1)
              }
          )
        )

-- | Spawn an entity with a component and its `ComponentID`.
spawnWithId :: (Typeable c) => ComponentID -> c -> World -> (EntityID, World)
spawnWithId cId c = spawnDyn cId (toDyn c)

-- | Spawn an entity with a dynamic component and its `ComponentID`.
spawnDyn :: ComponentID -> Dynamic -> World -> (EntityID, World)
spawnDyn cId c w = do
  let e = nextEntityID w
   in ( e,
        w
          { archetypes = AS.insertNewDyn e cId c (archetypes w),
            nextEntityID= EntityID(unEntityID e + 1)
          }
      )

-- | Insert a component into an `Entity`.
insert :: forall c. (Typeable c) => EntityID-> c -> World -> World
insert e c w = case CS.lookup @c (components w) of
  Just cId -> insertDyn e cId (toDyn c) w
  Nothing ->
    let (cId, cs) = CS.insert @c (components w)
        as = AS.insertUnchecked e cId c (archetypes w)
     in w {components = cs, archetypes = as}

-- | Insert a dynamic component into an `Entity`.
insertDyn :: EntityID-> ComponentID -> Dynamic -> World -> World
insertDyn e cId c w = w {archetypes = AS.insertDyn e cId c (archetypes w)}

-- | Lookup a component in an `Entity`.
lookup :: forall c. (Typeable c) => EntityID-> World -> Maybe c
lookup e w = do
  cId <- CS.lookup @c (components w)
  AS.lookupWithId e cId (archetypes w)

-- | Lookup a dynamic component in an `Entity`.
lookupDyn :: EntityID-> ComponentID -> World -> Maybe Dynamic
lookupDyn e cId w = AS.lookupDyn e cId (archetypes w)
