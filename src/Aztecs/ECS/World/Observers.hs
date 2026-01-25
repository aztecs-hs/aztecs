{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Aztecs.ECS.World.Observers
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Observers
  ( ObserverID (..),
    Observers (..),
    EntityObservers (..),
    DynamicObserver (..),
    empty,
    insertEntityObserver,
    insertEventObserver,
    addEntityObserver,
    addGlobalObserver,
    lookupEntityObservers,
    lookupGlobalObservers,
    lookupCallback,
    removeObserver,
  )
where

import Aztecs.ECS.Entity
import Aztecs.ECS.World.Observers.Internal
import Data.Dynamic
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable

-- | Empty `Observers`.
empty :: Observers m
empty =
  Observers
    { entityObservers' = mempty,
      globalObservers = mempty,
      observerCallbacks = mempty,
      nextObserverId = ObserverID 0
    }

-- | Insert an entity observer callback.
insertEntityObserver ::
  forall e m.
  (Typeable e, Monad m) =>
  (EntityID -> e -> m ()) ->
  Observers m ->
  (ObserverID, Observers m)
insertEntityObserver callback os =
  let !oId = nextObserverId os
      dynCallback eId dyn = case fromDynamic dyn of
        Just evt -> callback eId evt
        Nothing -> pure ()
   in ( oId,
        os
          { observerCallbacks = Map.insert oId (DynEntityObserver dynCallback) (observerCallbacks os),
            nextObserverId = ObserverID (unObserverId oId + 1)
          }
      )

-- | Insert an event observer callback.
insertEventObserver ::
  forall e m.
  (Typeable e, Monad m) =>
  (e -> m ()) ->
  Observers m ->
  (ObserverID, Observers m)
insertEventObserver callback os =
  let !oId = nextObserverId os
      dynCallback dyn = case fromDynamic dyn of
        Just evt -> callback evt
        Nothing -> pure ()
   in ( oId,
        os
          { observerCallbacks = Map.insert oId (DynEventObserver dynCallback) (observerCallbacks os),
            nextObserverId = ObserverID (unObserverId oId + 1)
          }
      )

-- | Add an observer to a specific entity for a given event type.
addEntityObserver :: forall m e. (Typeable e) => EntityID -> ObserverID -> Observers m -> Observers m
addEntityObserver e oId os =
  let eventTypeRep = typeOf (Proxy @e)
      updateEntityObs Nothing =
        Just $ EntityObservers {eventObservers = Map.singleton eventTypeRep (Set.singleton oId)}
      updateEntityObs (Just eo) =
        Just $ eo {eventObservers = Map.insertWith Set.union eventTypeRep (Set.singleton oId) (eventObservers eo)}
   in os {entityObservers' = Map.alter updateEntityObs e (entityObservers' os)}

-- | Add a global observer for a given event type.
addGlobalObserver :: forall m e. (Typeable e) => ObserverID -> Observers m -> Observers m
addGlobalObserver oId os =
  let eventTypeRep = typeOf (Proxy @e)
   in os {globalObservers = Map.insertWith Set.union eventTypeRep (Set.singleton oId) (globalObservers os)}

-- | Lookup all observer IDs for an entity and event type.
lookupEntityObservers :: TypeRep -> EntityID -> Observers m -> Set.Set ObserverID
lookupEntityObservers eventTypeRep e os =
  case Map.lookup e (entityObservers' os) of
    Just eo -> Map.findWithDefault Set.empty eventTypeRep (eventObservers eo)
    Nothing -> Set.empty

-- | Lookup all global observer IDs for an event type.
lookupGlobalObservers :: TypeRep -> Observers m -> Set.Set ObserverID
lookupGlobalObservers eventTypeRep os = Map.findWithDefault Set.empty eventTypeRep (globalObservers os)

-- | Lookup an observer callback by ID.
lookupCallback :: ObserverID -> Observers m -> Maybe (DynamicObserver m)
lookupCallback oId os = Map.lookup oId (observerCallbacks os)

-- | Remove an observer by ID.
removeObserver :: ObserverID -> Observers m -> Observers m
removeObserver oId os =
  os
    { observerCallbacks = Map.delete oId (observerCallbacks os),
      entityObservers' = Map.map removeFromEntity (entityObservers' os),
      globalObservers = Map.map (Set.delete oId) (globalObservers os)
    }
  where
    removeFromEntity eo = eo {eventObservers = Map.map (Set.delete oId) (eventObservers eo)}
