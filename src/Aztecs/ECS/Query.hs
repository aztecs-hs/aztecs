{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
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
-- Query for matching entities.
module Aztecs.ECS.Query
  ( -- * Queries
    Query (..),

    -- ** Operations
    entity,
    query,
    queryDyn,
    queryMap,
    queryMapAccum,

    -- ** Running
    runQuery',
    readQuery,
    readQuerySingle,
    runQuery,
    runQuerySingle,

    -- * Filters
    QueryFilter (..),
    with,
    without,

    -- * Reads and writes
    ReadsWrites (..),
    disjoint,

    -- * QueryStream
    QueryStream (..),

    -- * Re-exports
    DynamicQueryF,
  )
where

import Aztecs.ECS.Access.Internal (Access)
import Aztecs.ECS.Component
import Aztecs.ECS.Entity (EntityID)
import Aztecs.ECS.Query.Dynamic hiding (entity, queryDyn)
import qualified Aztecs.ECS.Query.Dynamic as DQ
import Aztecs.ECS.World.Archetype (Archetype)
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Applicative
import Control.Applicative.Free
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Prelude hiding (reads)

-- | A list with zip semantics for @Applicative@.
newtype QueryStream a = QueryStream {unQueryStream :: [a]}
  deriving (Functor, Show)

instance Applicative QueryStream where
  pure a = QueryStream [a]
  {-# INLINE pure #-}
  QueryStream fs <*> QueryStream xs = QueryStream (zipWith ($) fs xs)
  {-# INLINE (<*>) #-}

data Op f m a where
  EntityOp :: Op f m (f EntityID)
  QueryOp :: (Component m a) => ComponentID -> Op f m (f a)
  QueryMapOp :: (Component m a) => ComponentID -> (f a -> f a) -> Op f m (f a)
  QueryMapAccumOp :: (Component m b) => ComponentID -> (f b -> f (a, b)) -> Op f m (f (a, b))

newtype QueryBuilder f m a = QueryBuilder {unQueryBuilder :: Free (Ap (Op f m)) a}
  deriving (Functor, Applicative, Monad)

-- | Query for matching entities.
newtype Query f m a = Query {unQuery :: ReaderT Components (QueryBuilder f m) a}
  deriving (Functor, Applicative, Monad)

-- | Query the entity ID.
entity :: forall f m. (Applicative f) => Query f m (f EntityID)
entity = Query . lift . QueryBuilder . liftF . liftAp $ EntityOp
{-# INLINE entity #-}

-- | Query a component.
query :: forall f m a. (Applicative f, Component m a) => Query f m (f a)
query = Query $ do
  cs <- ask
  let !(cId, _) = CS.insert @a @m cs
  lift . QueryBuilder . liftF . liftAp $ QueryOp cId
{-# INLINE query #-}

-- | Query a component and update it.
queryMap :: forall f m a. (Applicative f, Component m a) => (f a -> f a) -> Query f m (f a)
queryMap f = Query $ do
  cs <- ask
  let !(cId, _) = CS.insert @a @m cs
  lift . QueryBuilder . liftF . liftAp $ QueryMapOp cId f
{-# INLINE queryMap #-}

-- | Query a component with input, returning a tuple of the result and the updated component.
queryMapAccum :: forall f m a b. (Applicative f, Component m b) => (f b -> f (a, b)) -> Query f m (f (a, b))
queryMapAccum f = Query $ do
  cs <- ask
  let !(cId, _) = CS.insert @b @m cs
  lift . QueryBuilder . liftF . liftAp $ QueryMapAccumOp cId f
{-# INLINE queryMapAccum #-}

newtype Fetch a = Fetch (Const () a)
  deriving (Functor, Applicative)

buildQueryBuilder :: (Monad m) => QueryBuilder QueryStream m (QueryStream a) -> DynamicQuery m a
buildQueryBuilder a = DynamicQuery $ \arch -> do
  (as, (arch', hooks)) <- runStateT (foldFree (runAp go) $ unQueryBuilder a) (arch, pure ())
  return (unQueryStream as, arch', hooks)
  where
    go :: (Monad m) => Op QueryStream m x -> StateT (Archetype m, Access m ()) m x
    go = \case
      EntityOp -> goEntity
      QueryOp cId -> goRead cId
      QueryMapOp cId f -> goWrite cId f
      QueryMapAccumOp cId f -> goMapAccum cId f
    goEntity :: (Monad m) => StateT (Archetype m, Access m ()) m (QueryStream EntityID)
    goEntity = do
      (arch, _) <- get
      return $ QueryStream $ Set.toList $ A.entities arch
    goRead :: forall m a. (Monad m, Component m a) => ComponentID -> StateT (Archetype m, Access m ()) m (QueryStream a)
    goRead cId = do
      (arch, hooks) <- get
      (as, arch', hooks') <- lift $ runDynQuery (DQ.queryDyn cId) arch
      put (arch', hooks >> hooks')
      return (QueryStream as)
    goWrite :: forall m a. (Monad m, Component m a) => ComponentID -> (QueryStream a -> QueryStream a) -> StateT (Archetype m, Access m ()) m (QueryStream a)
    goWrite cId f = do
      (arch, hooks) <- get
      let as = A.lookupComponentsAsc cId arch
          as' = unQueryStream $ f (QueryStream as)
          arch' = A.insertAscList cId as' arch
      put (arch', hooks)
      return (QueryStream as')
    goMapAccum :: forall m a b. (Monad m, Component m b) => ComponentID -> (QueryStream b -> QueryStream (a, b)) -> StateT (Archetype m, Access m ()) m (QueryStream (a, b))
    goMapAccum cId f = do
      (arch, hooks) <- get
      let bs = A.lookupComponentsAsc cId arch
          xs = unQueryStream $ f (QueryStream bs)
          arch' = A.insertAscList cId (fmap snd xs) arch
      put (arch', hooks)
      return (QueryStream xs)

runQuery' :: (Monad m) => (forall f. (Applicative f) => Query f m (f a)) -> Components -> (ReadsWrites, Components, DynamicQuery m a)
runQuery' qb cs =
  let actionFetch = runReaderT (unQuery qb) cs
      (_, rws) = runState (foldFree (runAp go) (unQueryBuilder actionFetch)) mempty
   in (rws, cs, buildQueryBuilder $ runReaderT (unQuery qb) cs)
  where
    go :: Op Fetch m x -> State ReadsWrites x
    go EntityOp = return (Fetch (Const ()))
    go (QueryOp cId) = do
      modify (\rws -> rws {reads = Set.insert cId (reads rws)})
      return (Fetch (Const ()))
    go (QueryMapOp cId _) = do
      modify (\rws -> rws {writes = Set.insert cId (writes rws)})
      return (Fetch (Const ()))
    go (QueryMapAccumOp cId _) = do
      modify (\rws -> rws {writes = Set.insert cId (writes rws)})
      return (Fetch (Const ()))

-- | Query a component dynamically by 'ComponentID'.
queryDyn :: forall f m a. (Applicative f, Component m a, Monad m) => ComponentID -> Query f m (f a)
queryDyn cId = Query . lift . QueryBuilder . liftF . liftAp $ QueryOp cId
{-# INLINE queryDyn #-}

-- | Read all matching entities.
readQuery :: (Monad m) => (forall f. (Applicative f) => Query f m (f a)) -> Entities m -> m ([a], Entities m)
readQuery q es =
  let (rws, cs', dynQ) = runQuery' q (components es)
   in do
        res <- DQ.readQueryDyn (reads rws <> writes rws) dynQ es
        return (res, es {components = cs'})
{-# INLINE readQuery #-}

-- | Read a single matching entity.
readQuerySingle :: (HasCallStack, Monad m) => (forall f. (Applicative f) => Query f m (f a)) -> Entities m -> m (a, Entities m)
readQuerySingle q es =
  let (rws, cs', dynQ) = runQuery' q (components es)
   in do
        res <- DQ.readQuerySingleDyn (reads rws <> writes rws) dynQ es
        return (res, es {components = cs'})
{-# INLINE readQuerySingle #-}

-- | Run a query on all matching entities, potentially modifying them.
runQuery :: (Monad m) => (forall f. (Applicative f) => Query f m (f a)) -> Entities m -> m ([a], Entities m, Access m ())
runQuery q es =
  let (rws, cs', dynQ) = runQuery' q $ components es
   in DQ.runQueryDyn (reads rws <> writes rws) dynQ es {components = cs'}
{-# INLINE runQuery #-}

-- | Run a query on a single matching entity, potentially modifying it.
runQuerySingle :: (HasCallStack, Monad m) => (forall f. (Applicative f) => Query f m (f a)) -> Entities m -> m (a, Entities m, Access m ())
runQuerySingle q es =
  let (rws, cs', dynQ) = runQuery' q $ components es
   in DQ.runQuerySingleDyn (reads rws <> writes rws) dynQ es {components = cs'}
{-# INLINE runQuerySingle #-}

-- | Reads and writes of a `Query`.
data ReadsWrites = ReadsWrites
  { -- | Component IDs being read.
    reads :: !(Set ComponentID),
    -- | Component IDs being written.
    writes :: !(Set ComponentID)
  }
  deriving (Show)

instance Semigroup ReadsWrites where
  ReadsWrites r1 w1 <> ReadsWrites r2 w2 = ReadsWrites (r1 <> r2) (w1 <> w2)
  {-# INLINE (<>) #-}

instance Monoid ReadsWrites where
  mempty = ReadsWrites mempty mempty
  {-# INLINE mempty #-}

-- | `True` if the reads and writes of two `Query`s overlap.
disjoint :: ReadsWrites -> ReadsWrites -> Bool
disjoint a b =
  Set.disjoint (reads a) (writes b)
    || Set.disjoint (reads b) (writes a)
    || Set.disjoint (writes b) (writes a)

-- | Filter for a `Query`.
newtype QueryFilter = QueryFilter
  { -- | Run a query filter.
    runQueryFilter :: Components -> (DynamicQueryFilter, Components)
  }

instance Semigroup QueryFilter where
  a <> b =
    QueryFilter
      ( \cs ->
          let !(withA', cs') = runQueryFilter a cs
              !(withB', cs'') = runQueryFilter b cs'
           in (withA' <> withB', cs'')
      )

instance Monoid QueryFilter where
  mempty = QueryFilter (mempty,)

-- | Filter for entities containing this component.
with :: forall m a. (Component m a) => QueryFilter
with = QueryFilter $ \cs ->
  let !(cId, cs') = CS.insert @a @m cs in (mempty {filterWith = Set.singleton cId}, cs')

-- | Filter out entities containing this component.
without :: forall m a. (Component m a) => QueryFilter
without = QueryFilter $ \cs ->
  let !(cId, cs') = CS.insert @a @m cs in (mempty {filterWithout = Set.singleton cId}, cs')
