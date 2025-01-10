{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs
  ( Entity (..),
    ComponentID (..),
    World (..),
    empty,
    spawn,
    spawnWithId,
    insert,
    lookup,
  )
where

import Data.Aztecs.Archetypes (Archetypes, Entity (..))
import qualified Data.Aztecs.Archetypes as AS
import Data.Aztecs.Components (ComponentID (..), Components)
import qualified Data.Aztecs.Components as CS
import Data.Data (Typeable)
import Prelude hiding (lookup)

-- | World of entities and components.
data World = World
  { archetypes :: Archetypes,
    components :: Components,
    nextEntity :: Entity
  }
  deriving (Show)

-- | Empty world.
empty :: World
empty =
  World
    { archetypes = AS.empty,
      components = CS.empty,
      nextEntity = Entity 0
    }

-- | Spawn an entity with a component.
spawn :: forall c. (Typeable c) => c -> World -> (Entity, World)
spawn c w = case CS.lookup @c (components w) of
  Just cId -> spawnWithId cId c w
  Nothing ->
    let (cId, cs) = CS.insert @c (components w)
        w' = w {components = cs}
        e = nextEntity w'
     in ( e,
          ( w'
              { archetypes = AS.insertNewComponent e cId c (archetypes w),
                nextEntity = Entity (unEntity e + 1)
              }
          )
        )

-- | Spawn an entity with a component and its `ComponentID`.
spawnWithId :: (Typeable c) => ComponentID -> c -> World -> (Entity, World)
spawnWithId cId c w = do
  let e = nextEntity w
   in ( e,
        w
          { archetypes = AS.insertNew e cId c (archetypes w),
            nextEntity = Entity (unEntity e + 1)
          }
      )

-- | Insert a component into an `Entity`.
insert :: forall c. (Typeable c) => Entity -> c -> World -> World
insert e c w = case CS.lookup @c (components w) of
  Just cId -> w {archetypes = AS.insert e cId c (archetypes w)}
  Nothing ->
    let (cId, cs) = CS.insert @c (components w)
        as = AS.insertUnchecked e cId c (archetypes w)
     in w {components = cs, archetypes = as}

-- | Lookup a component in an `Entity`.
lookup :: forall c. (Typeable c) => Entity -> World -> Maybe c
lookup e w = case CS.lookup @c (components w) of
  Just cId -> AS.lookupWithId e cId (archetypes w)
  Nothing -> Nothing
