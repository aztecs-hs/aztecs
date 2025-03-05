{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.ECS.Query
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Query for matching entities.
--
-- === Do notation:
-- > move :: (ArrowQuery arr) => arr () Position
-- > move = proc () -> do
-- >   Velocity v <- Q.fetch -< ()
-- >   Position p <- Q.fetch -< ()
-- >   Q.set -< Position $ p + v
--
-- === Arrow combinators:
-- > move :: (ArrowQuery arr) => arr () Position
-- > move = Q.fetch &&& Q.fetch >>> arr (\(Position p, Velocity v) -> Position $ p + v) >>> Q.set
--
-- === Applicative combinators:
-- > move :: (ArrowQuery arr) => arr () Position
-- > move = (,) <$> Q.fetch <*> Q.fetch >>> arr (\(Position p, Velocity v) -> Position $ p + v) >>> Q.set
module Aztecs.ECS.Query
  ( -- * Queries
    Query,
    QueryT (..),
    QueryReaderF (..),
    QueryF (..),
    DynamicQueryReaderF (..),
    DynamicQueryF (..),

    -- ** Running
    all,
    all',
    single,
    single',
    singleMaybe,
    singleMaybe',
    map,
    mapSingle,
    mapSingleMaybe,

    -- ** Conversion
    fromReader,
    toReader,

    -- * Filters
    QueryFilter (..),
    with,
    without,

    -- * Reads and writes
    ReadsWrites (..),
    disjoint,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Class
import Aztecs.ECS.Query.Dynamic
import Aztecs.ECS.Query.Reader (QueryFilter (..), QueryReader (..), with, without)
import qualified Aztecs.ECS.Query.Reader as QR
import Aztecs.ECS.Query.Reader.Class
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Category
import Control.Monad.Identity
import Control.Monad.Writer
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack
import Prelude hiding (all, id, map, reads, (.))

-- | @since 0.10
type Query = QueryT Identity

-- | Query for matching entities.
--
-- @since 0.10
newtype QueryT m a = Query
  { -- | Run a query, producing a `DynamicQueryT`.
    --
    -- @since 0.10
    runQuery :: Components -> (ReadsWrites, Components, DynamicQueryT m a)
  }
  deriving (Functor)

-- | @since 0.10
instance (Monad m) => Applicative (QueryT m) where
  {-# INLINE pure #-}
  pure a = Query (mempty,,pure a)

  {-# INLINE (<*>) #-}
  (Query f) <*> (Query g) = Query $ \cs ->
    let !(cIdsG, cs', aQS) = g cs
        !(cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS <*> aQS)

-- | @since 0.10
instance (Monad m) => QueryReaderF (QueryT m) where
  {-# INLINE fetch #-}
  fetch = fromReader fetch

  {-# INLINE fetchMaybe #-}
  fetchMaybe = fromReader fetchMaybe

-- | @since 0.10
instance (Monad m) => DynamicQueryReaderF (QueryT m) where
  {-# INLINE entity #-}
  entity = fromReader entity

  {-# INLINE fetchDyn #-}
  fetchDyn = fromReader . fetchDyn

  {-# INLINE fetchMaybeDyn #-}
  fetchMaybeDyn = fromReader . fetchMaybeDyn

-- | @since 0.10
instance (Monad m) => DynamicQueryF m (QueryT m) where
  {-# INLINE adjustDyn #-}
  adjustDyn f cId q = Query $ \cs ->
    let !(rws, cs', dynQ) = runQuery q cs in (rws, cs', adjustDyn f cId dynQ)

  {-# INLINE adjustDyn_ #-}
  adjustDyn_ f cId q = Query $ \cs ->
    let !(rws, cs', dynQ) = runQuery q cs in (rws, cs', adjustDyn_ f cId dynQ)

  {-# INLINE adjustDynM #-}
  adjustDynM f cId q = Query $ \cs ->
    let !(rws, cs', dynQ) = runQuery q cs in (rws, cs', adjustDynM f cId dynQ)

  {-# INLINE setDyn #-}
  setDyn cId q = Query $ \cs ->
    let !(rws, cs', dynQ) = runQuery q cs in (rws, cs', setDyn cId dynQ)

-- | @since 0.9
instance (Monad m) => QueryF m (QueryT m) where
  {-# INLINE adjust #-}
  adjust :: forall a b. (Component a) => (b -> a -> a) -> QueryT m b -> QueryT m a
  adjust f q = Query $ \cs ->
    let !(cId, cs') = CS.insert @a cs
        !(rws, cs'', dynQ) = runQuery q cs'
     in (rws, cs'', adjustDyn f cId dynQ)

  {-# INLINE adjust_ #-}
  adjust_ :: forall a b. (Component a) => (b -> a -> a) -> QueryT m b -> QueryT m ()
  adjust_ f q = Query $ \cs ->
    let !(cId, cs') = CS.insert @a cs
        !(rws, cs'', dynQ) = runQuery q cs'
     in (rws, cs'', adjustDyn_ f cId dynQ)

  {-# INLINE adjustM #-}
  adjustM :: forall a b. (Component a, Monad m) => (b -> a -> m a) -> QueryT m b -> QueryT m a
  adjustM f q = Query $ \cs ->
    let !(cId, cs') = CS.insert @a cs
        !(rws, cs'', dynQ) = runQuery q cs'
     in (rws, cs'', adjustDynM f cId dynQ)

  {-# INLINE set #-}
  set :: forall a. (Component a) => QueryT m a -> QueryT m a
  set q = Query $ \cs ->
    let !(cId, cs') = CS.insert @a cs
        !(rws, cs'', dynQ) = runQuery q cs'
     in (rws, cs'', setDyn cId dynQ)

-- | Convert a `QueryReader` to a `Query`.
--
-- @since 0.9
{-# INLINE fromReader #-}
fromReader :: (Monad m) => QueryReader a -> QueryT m a
fromReader (QueryReader f) = Query $ \cs ->
  let !(cIds, cs', dynQ) = f cs in (ReadsWrites cIds Set.empty, cs', fromDynReader dynQ)

-- | Convert a `Query` to a `QueryReader`.
--
-- @since 0.10
{-# INLINE toReader #-}
toReader :: Query a -> QueryReader a
toReader (Query f) = QueryReader $ \cs ->
  let !(rws, cs', dynQ) = f cs in (reads rws, cs', toDynReader dynQ)

-- | Reads and writes of a `Query`.
--
-- @since 0.9
data ReadsWrites = ReadsWrites
  { -- | Component IDs being read.
    --
    -- @since 0.9
    reads :: !(Set ComponentID),
    -- | Component IDs being written.
    --
    -- @since 0.9
    writes :: !(Set ComponentID)
  }
  deriving (Show)

-- | @since 0.9
instance Semigroup ReadsWrites where
  ReadsWrites r1 w1 <> ReadsWrites r2 w2 = ReadsWrites (r1 <> r2) (w1 <> w2)

-- | @since 0.9
instance Monoid ReadsWrites where
  mempty = ReadsWrites mempty mempty

-- | `True` if the reads and writes of two `Query`s overlap.
--
-- @since 0.9
disjoint :: ReadsWrites -> ReadsWrites -> Bool
disjoint a b =
  Set.disjoint (reads a) (writes b)
    || Set.disjoint (reads b) (writes a)
    || Set.disjoint (writes b) (writes a)

-- | Match all entities.
--
-- @since 0.9
{-# INLINE all #-}
all :: Query a -> Entities -> ([a], Entities)
all = QR.all . toReader

-- | Match all entities.
--
-- @since 0.9
{-# INLINE all' #-}
all' :: Query a -> Entities -> ([a], Components)
all' = QR.all' . toReader

-- | Match a single entity.
--
-- @since 0.9
{-# INLINE single #-}
single :: (HasCallStack) => Query a -> Entities -> (a, Entities)
single = QR.single . toReader

-- | Match a single entity.
--
-- @since 0.9
{-# INLINE single' #-}
single' :: (HasCallStack) => Query a -> Entities -> (a, Components)
single' = QR.single' . toReader

-- | Match a single entity, or `Nothing`.
--
-- @since 0.9
{-# INLINE singleMaybe #-}
singleMaybe :: Query a -> Entities -> (Maybe a, Entities)
singleMaybe = QR.singleMaybe . toReader

-- | Match a single entity, or `Nothing`.
--
-- @since 0.9
{-# INLINE singleMaybe' #-}
singleMaybe' :: Query a -> Entities -> (Maybe a, Components)
singleMaybe' = QR.singleMaybe' . toReader

-- | Map all matched entities.
--
-- @since 0.9
{-# INLINE map #-}
map :: (Monad m) => QueryT m o -> Entities -> m ([o], Entities)
map q es = do
  let !(rws, cs', dynQ) = runQuery q $ components es
      !cIds = reads rws <> writes rws
  (as, es') <- mapDyn cIds dynQ es
  return (as, es' {components = cs'})

-- | Map a single matched entity.
--
-- @since 0.9
{-# INLINE mapSingle #-}
mapSingle :: (HasCallStack, Monad m) => QueryT m a -> Entities -> m (a, Entities)
mapSingle q es = do
  let !(rws, cs', dynQ) = runQuery q $ components es
      !cIds = reads rws <> writes rws
  (as, es') <- mapSingleDyn cIds dynQ es
  return (as, es' {components = cs'})

-- | Map a single matched entity, or `Nothing`.
--
-- @since 0.9
{-# INLINE mapSingleMaybe #-}
mapSingleMaybe :: (Monad m) => QueryT m a -> Entities -> m (Maybe a, Entities)
mapSingleMaybe q es = do
  let !(rws, cs', dynQ) = runQuery q $ components es
      !cIds = reads rws <> writes rws
  (as, es') <- mapSingleMaybeDyn cIds dynQ es
  return (as, es' {components = cs'})
