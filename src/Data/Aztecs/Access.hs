{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Access
  ( Access (..),
    runAccess,
    spawn,
    spawn_,
    insert,
    all,
    map,
    lookup,
    lookupQuery,
    alter,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState (..), StateT (..), gets)
import Data.Aztecs.Component (Component (..))
import Data.Aztecs.Entity (ComponentIds, Entity, EntityID, EntityT, FromEntity (..), ToEntity)
import Data.Aztecs.Query (IsEq, Map, Query, Queryable (..))
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.World (World)
import qualified Data.Aztecs.World as W
import Data.Aztecs.World.Archetype (Insert)
import Data.Data (Typeable)
import Prelude hiding (all, lookup, map)

-- | Access into the `World`.
newtype Access m a = Access {unAccess :: StateT World m a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run an `Access` on a `World`, returning the output and updated `World`.
runAccess :: Access m a -> World -> m (a, World)
runAccess a = runStateT (unAccess a)

-- | Spawn an entity with a component.
spawn ::
  (Monad m, ComponentIds (EntityT a), ToEntity a, Insert (Entity (EntityT a))) =>
  a ->
  Access m EntityID
spawn c = Access $ do
  w <- get
  let (e, w') = W.spawn c w
  put w'
  return e

spawn_ :: (Monad m, ComponentIds (EntityT a), ToEntity a, Insert (Entity (EntityT a))) => a -> Access m ()
spawn_ c = do
  _ <- spawn c
  return ()

-- | Insert a component into an entity.
insert :: (Monad m, Component a, Typeable (StorageT a)) => EntityID -> a -> Access m ()
insert e c = Access $ do
  w <- get
  let w' = W.insert e c w
  put w'

all :: forall m a. (Monad m, ToEntity a, FromEntity a, Queryable (EntityT a)) => Access m [(EntityID, a)]
all = Access $ gets (fmap (\(eId, e) -> (eId, fromEntity e)) . Q.queryAll (query @(EntityT a)))

-- | Map over all entities that match this query,
-- storing the resulting components in the @World@.
map ::
  forall m i o.
  (Monad m, Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o) =>
  (i -> o) ->
  Access m [o]
map f = Access $ do
  w <- get
  let (out, w') = Q.map @i @o f w
  put w'
  return out

alter :: forall a m. (Monad m, FromEntity a, ToEntity a, Queryable (EntityT a)) => EntityID -> (a -> a) -> Access m ()
alter eId f = Access $ do
  w <- get
  let w' = Q.alter eId f w
  put w'

lookup :: forall a m. (Monad m, FromEntity a, Queryable (EntityT a)) => EntityID -> Access m (Maybe a)
lookup eId = Access . gets $ Q.lookup eId

lookupQuery :: (Monad m) => EntityID -> Query a -> Access m (Maybe (Entity a))
lookupQuery eId q = Access . gets $ Q.lookupQuery eId q
