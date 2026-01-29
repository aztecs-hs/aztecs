{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.ECS.Observer
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Observer
  ( Observer (..),
    ObserverID (..),
    ObserverKind (..),
    observer,
    observerFor,
    observerGlobal,
  )
where

import Aztecs.ECS.Access.Internal
import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.Event
import qualified Aztecs.ECS.World as W
import Aztecs.ECS.World.Bundle
import Aztecs.ECS.World.Internal (World (..))
import qualified Aztecs.ECS.World.Observers as O
import Aztecs.ECS.World.Observers.Internal
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable

-- | The kind of observer - either entity-specific or global.
data ObserverKind m e
  = -- | Observe events on specific entities (callback receives EntityID and event).
    EntityObserver !(Set EntityID) !(EntityID -> e -> Access m ())
  | -- | Observe all events of this type globally (callback receives just the event).
    EventObserver !(e -> Access m ())

-- | Observer component
data Observer m e = Observer
  { -- | The kind and callback for this observer.
    observerKind :: !(ObserverKind m e),
    -- | The ObserverID assigned after registration (Nothing before registration).
    observerId :: !(Maybe ObserverID)
  }

instance Show (ObserverKind m e) where
  show (EntityObserver targets _) = "EntityObserver " ++ show targets
  show (EventObserver _) = "EventObserver"

instance Show (Observer m e) where
  show o = "Observer { kind = " ++ show (observerKind o) ++ ", id = " ++ show (observerId o) ++ " }"

instance (Monad m, Typeable m, Event e) => Component m (Observer m e) where
  type StorageT (Observer m e) = [Observer m e]

  componentOnInsert ownerEntity o = Access $ do
    !w <- get
    (oId, observers') <- case observerKind o of
      EntityObserver targets f -> do
        let f' eId evt = unAccess $ f eId evt
            (oId, observers') = O.insertEntityObserver @e f' $ observers w
            observers'' = foldr (\e os -> O.addEntityObserver @_ @e e oId os) observers' targets
        return (oId, observers'')
      EventObserver callback -> do
        let wrappedCallback evt = unAccess $ callback evt
            (oId, observers') = O.insertEventObserver @e wrappedCallback (observers w)
            observers'' = O.addGlobalObserver @_ @e oId observers'
        return (oId, observers'')
    let o' = o {observerId = Just oId}
        w' = W.insertUntracked ownerEntity (bundleUntracked o') w {observers = observers'}
    put w'

  componentOnRemove _ownerEntity o = Access $ case observerId o of
    Just oId -> do
      !w <- get
      put w {observers = O.removeObserver oId (observers w)}
    Nothing -> pure ()

-- | Create an observer for specific entities.
observerFor :: forall m e. (Event e) => Set EntityID -> (EntityID -> e -> Access m ()) -> Observer m e
observerFor targets callback = Observer {observerKind = EntityObserver targets callback, observerId = Nothing}

-- | Create a global observer (observes all events of this type).
observerGlobal :: forall m e. (Event e) => (e -> Access m ()) -> Observer m e
observerGlobal callback = Observer {observerKind = EventObserver callback, observerId = Nothing}

-- | Create an observer for a single entity.
observer :: forall m e. (Event e) => EntityID -> (EntityID -> e -> Access m ()) -> Observer m e
observer e = observerFor (Set.singleton e)
