{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Aztecs.ECS.World.Observers.Internal
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Observers.Internal
  ( ObserverID (..),
    Observers (..),
    EntityObservers (..),
    DynamicObserver (..),
  )
where

import Aztecs.ECS.Entity
import Data.Dynamic
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Typeable
import GHC.Generics

-- | Observer identifier
newtype ObserverID = ObserverID
  { -- | Unique integer identifier
    unObserverId :: Int
  }
  deriving (Eq, Ord, Show, Generic)

-- | Dynamic observer callback
data DynamicObserver m
  = -- | Entity observer callback
    DynEntityObserver !(EntityID -> Dynamic -> m ())
  | -- | Event observer callback
    DynEventObserver !(Dynamic -> m ())

-- | Observers for a specific entity
newtype EntityObservers = EntityObservers {eventObservers :: Map TypeRep (Set ObserverID)}
  deriving (Show, Generic, Semigroup, Monoid)

-- | Global observer storage
data Observers m = Observers
  { entityObservers' :: !(Map EntityID EntityObservers),
    globalObservers :: !(Map TypeRep (Set ObserverID)),
    observerCallbacks :: !(Map ObserverID (DynamicObserver m)),
    nextObserverId :: !ObserverID
  }
