{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.ECS.Query.Dynamic
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Query.Dynamic
  ( -- * Dynamic queries
    DynamicQuery,
    DynamicQueryT (..),
    DynamicQueryReaderF (..),
    DynamicQueryF (..),

    -- ** Conversion
    fromDynReader,
    toDynReader,

    -- ** Running
    mapDyn,
    filterMapDyn,
    mapSingleDyn,
    mapSingleMaybeDyn,

    -- * Dynamic query filters
    DynamicQueryFilter (..),

    -- * Reads and writes
    ReadsWrites (..),
    disjoint,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic.Class
import Aztecs.ECS.Query.Dynamic.Reader
import Aztecs.ECS.World.Archetype (Archetype)
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (ArchetypeID, Node (..))
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Monad.Identity
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack
import Prelude hiding (reads)

type DynamicQuery = DynamicQueryT Identity

-- | Dynamic query for components by ID.
--
-- @since 0.11
newtype DynamicQueryT f a
  = DynamicQuery
  { -- | Run a dynamic query.
    --
    -- @since 0.11
    unDynQuery :: (ReadsWrites, Archetype -> f ([a], Archetype))
  }
  deriving (Functor)

-- | @since 0.10
instance (Applicative f) => Applicative (DynamicQueryT f) where
  {-# INLINE pure #-}
  pure a = DynamicQuery (mempty, \arch -> pure (replicate (length $ A.entities arch) a, arch))

  {-# INLINE (<*>) #-}
  (DynamicQuery (rws, f)) <*> (DynamicQuery (rws', g)) =
    DynamicQuery
      ( rws <> rws',
        \arch -> do
          x <- g arch
          y <- f arch
          return $
            let (as, arch') = x
                (bs, arch'') = y
             in (zipWith ($) bs as, arch' <> arch'')
      )

-- | @since 0.10
instance DynamicQueryReaderF DynamicQuery where
  {-# INLINE entity #-}
  entity = fromDynReader entity

  {-# INLINE fetchDyn #-}
  fetchDyn = fromDynReader . fetchDyn

  {-# INLINE fetchMaybeDyn #-}
  fetchMaybeDyn = fromDynReader . fetchMaybeDyn

-- | @since 0.10
instance (Applicative f) => DynamicQueryF f (DynamicQueryT f) where
  {-# INLINE adjustDyn #-}
  adjustDyn f cId (DynamicQuery (rws, q)) =
    DynamicQuery
      ( rws <> ReadsWrites Set.empty (Set.singleton cId),
        fmap (\(bs, arch') -> A.zipWith bs f cId arch') . q
      )

  {-# INLINE adjustDyn_ #-}
  adjustDyn_ f cId (DynamicQuery (rws, q)) =
    DynamicQuery
      ( rws <> ReadsWrites Set.empty (Set.singleton cId),
        fmap (\(bs, arch') -> (map (const ()) bs, A.zipWith_ bs f cId arch')) . q
      )

  {-# INLINE adjustDynM #-}
  adjustDynM f cId (DynamicQuery (rws, q)) =
    DynamicQuery
      ( rws <> ReadsWrites Set.empty (Set.singleton cId),
        \arch -> do
          (bs, arch') <- q arch
          A.zipWithM bs f cId arch'
      )

  {-# INLINE setDyn #-}
  setDyn cId (DynamicQuery (rws, q)) =
    DynamicQuery
      ( rws <> ReadsWrites Set.empty (Set.singleton cId),
        fmap (\(bs, arch') -> (bs, A.insertAscList cId bs arch')) . q
      )

-- | Convert a `DynamicQueryReaderT` to a `DynamicQueryT`.
--
-- @since 0.10
{-# INLINE fromDynReader #-}
fromDynReader :: (Applicative m) => DynamicQueryReader a -> DynamicQueryT m a
fromDynReader (DynamicQueryReader (rs, q)) =
  DynamicQuery (ReadsWrites rs Set.empty, \arch -> pure (q arch, arch))

-- | Convert a `DynamicQueryT` to a `DynamicQueryReaderT`.
--
-- @since 0.10
{-# INLINE toDynReader #-}
toDynReader :: DynamicQuery a -> DynamicQueryReader a
toDynReader (DynamicQuery (ReadsWrites rs ws, q)) =
  DynamicQueryReader (rs <> ws, fst . runIdentity . q)

-- | Map all matched entities.
--
-- @since 0.10
{-# INLINE mapDyn #-}
mapDyn :: (Monad m) => DynamicQueryT m a -> Entities -> m ([a], Entities)
mapDyn = mapDyn' id

-- | Map all matched entities with a filter.
--
-- @since 0.10
{-# INLINE filterMapDyn #-}
filterMapDyn ::
  (Monad m) =>
  (Node -> Bool) ->
  DynamicQueryT m a ->
  Entities ->
  m ([a], Entities)
filterMapDyn f = mapDyn' (Map.filter f)

{-# INLINE mapDyn' #-}
mapDyn' ::
  (Monad m) =>
  (Map ArchetypeID Node -> Map ArchetypeID Node) ->
  DynamicQueryT m a ->
  Entities ->
  m ([a], Entities)
mapDyn' f (DynamicQuery (ReadsWrites rs ws, q)) es =
  let cIds = rs <> ws
      go = q
   in if Set.null cIds
        then do
          (as, _) <- go A.empty {A.entities = Map.keysSet $ entities es}
          return (as, es)
        else
          let go' (acc, esAcc) (aId, n) = do
                (as', arch') <- go $ nodeArchetype n
                let n' = n {nodeArchetype = arch' <> nodeArchetype n}
                    !nodes = Map.insert aId n' . AS.nodes $ archetypes esAcc
                return (as' ++ acc, esAcc {archetypes = (archetypes esAcc) {AS.nodes = nodes}})
           in foldlM go' ([], es) $ Map.toList . f . AS.find cIds $ archetypes es

-- | Map a single matched entity.
--
-- @since 0.10
mapSingleDyn :: (HasCallStack, Monad m) => DynamicQueryT m a -> Entities -> m (a, Entities)
mapSingleDyn q es = do
  res <- mapSingleMaybeDyn q es
  return $ case res of
    (Just a, es') -> (a, es')
    _ -> error "mapSingleDyn: expected single matching entity"

-- | Map a single matched entity, or @Nothing@.
--
-- @since 0.10
{-# INLINE mapSingleMaybeDyn #-}
mapSingleMaybeDyn :: (Monad m) => DynamicQueryT m a -> Entities -> m (Maybe a, Entities)
mapSingleMaybeDyn (DynamicQuery (ReadsWrites rs ws, q)) es =
  let cIds = rs <> ws
   in if Set.null cIds
        then case Map.keys $ entities es of
          [eId] -> do
            res <- q $ A.singleton eId
            return $ case res of
              ([a], _) -> (Just a, es)
              _ -> (Nothing, es)
          _ -> pure (Nothing, es)
        else case Map.toList $ AS.find cIds $ archetypes es of
          [(aId, n)] -> do
            res <- q $ AS.nodeArchetype n
            return $ case res of
              ([a], arch') ->
                let nodes = Map.insert aId n {nodeArchetype = arch' <> nodeArchetype n} . AS.nodes $ archetypes es
                 in (Just a, es {archetypes = (archetypes es) {AS.nodes = nodes}})
              _ -> (Nothing, es)
          _ -> pure (Nothing, es)

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
