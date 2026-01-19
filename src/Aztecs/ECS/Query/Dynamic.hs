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
    DynamicQueryF (..),

    -- ** Running
    runDynQuery,
    readQueryDyn,
    readQueryDynM,
    readQueryFilteredDyn,
    readQueryFilteredDynM,
    readQuerySingleDyn,
    readQuerySingleDynM,
    readQuerySingleMaybeDyn,
    readQuerySingleMaybeDynM,
    queryDyn,
    queryDynM,
    queryFilteredDyn,
    queryFilteredDynM,
    querySingleDyn,
    querySingleDynM,
    querySingleMaybeDyn,
    querySingleMaybeDynM,

    -- * Dynamic query filters
    DynamicQueryFilter (..),
  )
where

import Aztecs.ECS.Access.Internal (AccessT)
import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic.Class
import Aztecs.ECS.World.Archetype (Archetype)
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Monad.Identity
import Data.Foldable
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Stack

type DynamicQuery = DynamicQueryT Identity

-- | Dynamic query for components by ID.
newtype DynamicQueryT m a
  = DynamicQuery
  { -- | Run a dynamic query.
    --
    -- @since 0.10
    runDynQueryT :: Archetype m -> m (Vector a, Archetype m, AccessT m ())
  }
  deriving (Functor)

instance (Monad m) => Applicative (DynamicQueryT m) where
  pure a = DynamicQuery $ \arch -> pure (V.replicate (length $ A.entities arch) a, arch, return ())
  {-# INLINE pure #-}

  f <*> g = DynamicQuery $ \arch -> do
    x <- runDynQueryT g arch
    y <- runDynQueryT f arch
    return $
      let (as, arch', hook1) = x
          (bs, arch'', hook2) = y
       in (V.zipWith ($) bs as, arch' <> arch'', hook1 >> hook2)
  {-# INLINE (<*>) #-}

instance (Monad m) => DynamicQueryF m (DynamicQueryT m) where
  entity = DynamicQuery $ \arch -> pure (V.fromList . Set.toList $ A.entities arch, arch, return ())
  {-# INLINE entity #-}

  fetchDyn cId = DynamicQuery $ \arch -> pure (A.lookupComponentsAsc cId arch, arch, return ())
  {-# INLINE fetchDyn #-}

  fetchMaybeDyn cId = DynamicQuery $ \arch -> case A.lookupComponentsAscMaybe cId arch of
    Just as -> pure (V.map Just as, arch, return ())
    Nothing -> pure (V.replicate (length $ A.entities arch) Nothing, arch, return ())
  {-# INLINE fetchMaybeDyn #-}

  adjustDyn f cId q = DynamicQuery $ \arch -> do
    (bs, arch', hook1) <- runDynQueryT q arch
    let (cs, arch'', hook2) = A.zipWith bs f cId arch'
    return (cs, arch'', hook1 >> hook2)
  {-# INLINE adjustDyn #-}

  adjustDyn_ f cId q = DynamicQuery $ \arch -> do
    (bs, arch', hook1) <- runDynQueryT q arch
    let (arch'', hook2) = A.zipWith_ bs f cId arch'
    return (V.map (const ()) bs, arch'', hook1 >> hook2)
  {-# INLINE adjustDyn_ #-}

  adjustDynM f cId q = DynamicQuery $ \arch -> do
    (bs, arch', hook1) <- runDynQueryT q arch
    (cs, arch'', hook2) <- A.zipWithM bs f cId arch'
    return (cs, arch'', hook1 >> hook2)
  {-# INLINE adjustDynM #-}

  setDyn cId q =
    DynamicQuery (fmap (\(bs, arch', hook) -> (bs, A.insertAscVector cId bs arch', hook)) . runDynQueryT q)
  {-# INLINE setDyn #-}

runDynQuery :: DynamicQuery a -> Archetype Identity -> (Vector a, Archetype Identity, AccessT Identity ())
runDynQuery q = runIdentity . runDynQueryT q

-- | Match all entities.
readQueryDyn :: Set ComponentID -> DynamicQuery a -> Entities Identity -> (Vector a)
readQueryDyn cIds q es = runIdentity $ readQueryDynM cIds q es

-- | Match all entities.
readQueryDynM :: (Monad m) => Set ComponentID -> DynamicQueryT m a -> Entities m -> m (Vector a)
readQueryDynM cIds q es =
  if Set.null cIds
    then (\(a, _, _) -> a) <$> runDynQueryT q A.empty {A.entities = Map.keysSet $ entities es}
    else do
      let go n = (\(a, _, _) -> a) <$> runDynQueryT q (AS.nodeArchetype n)
      results <- mapM go . Map.elems $ AS.find cIds $ archetypes es
      return $ V.concat results

readQueryFilteredDyn :: Set ComponentID -> DynamicQuery a -> (Node Identity -> Bool) -> Entities Identity -> (Vector a)
readQueryFilteredDyn cIds q f = runIdentity . readQueryFilteredDynM cIds f q

-- | Match all entities with a filter.
readQueryFilteredDynM :: (Monad m) => Set ComponentID -> (Node m -> Bool) -> DynamicQueryT m a -> Entities m -> m (Vector a)
readQueryFilteredDynM cIds f q es =
  if Set.null cIds
    then (\(a, _, _) -> a) <$> runDynQueryT q A.empty {A.entities = Map.keysSet $ entities es}
    else do
      let go n = (\(a, _, _) -> a) <$> runDynQueryT q (AS.nodeArchetype n)
      results <- mapM go . Map.elems . Map.filter f $ AS.find cIds $ archetypes es
      return $ V.concat results

-- | Match a single entity.
readQuerySingleDyn :: (HasCallStack) => Set ComponentID -> DynamicQuery a -> Entities Identity -> a
readQuerySingleDyn cIds q = runIdentity . readQuerySingleDynM cIds q

-- | Match a single entity.
readQuerySingleDynM :: (HasCallStack, Monad m) => Set ComponentID -> DynamicQueryT m a -> Entities m -> m a
readQuerySingleDynM cIds q es = do
  res <- readQuerySingleMaybeDynM cIds q es
  case res of
    Just a -> return a
    _ -> error "readQuerySingleDyn: expected a single entity"

-- | Match a single entity, or `Nothing`.
readQuerySingleMaybeDyn :: Set ComponentID -> DynamicQuery a -> Entities Identity -> Maybe a
readQuerySingleMaybeDyn cIds q = runIdentity . readQuerySingleMaybeDynM cIds q

-- | Match a single entity, or `Nothing`.
readQuerySingleMaybeDynM :: (Monad m) => Set ComponentID -> DynamicQueryT m a -> Entities m -> m (Maybe a)
readQuerySingleMaybeDynM cIds q es =
  if Set.null cIds
    then case Map.keys $ entities es of
      [eId] -> do
        (v, _, _) <- runDynQueryT q $ A.singleton eId
        return $ if V.length v == 1 then Just (V.head v) else Nothing
      _ -> return Nothing
    else case Map.elems $ AS.find cIds $ archetypes es of
      [n] -> do
        (v, _, _) <- runDynQueryT q $ AS.nodeArchetype n
        return $ if V.length v == 1 then Just (V.head v) else Nothing
      _ -> return Nothing

-- | Map all matched entities.
queryDyn :: Set ComponentID -> DynamicQuery a -> Entities Identity -> (Vector a, Entities Identity, AccessT Identity ())
queryDyn cIds q = runIdentity . queryDynM cIds q
{-# INLINE queryDyn #-}

-- | Map all matched entities.
queryDynM :: (Monad m) => Set ComponentID -> DynamicQueryT m a -> Entities m -> m (Vector a, Entities m, AccessT m ())
queryDynM cIds q es =
  let go = runDynQueryT q
   in if Set.null cIds
        then do
          (as, _, hook) <- go A.empty {A.entities = Map.keysSet $ entities es}
          return (as, es, hook)
        else
          let go' (acc, esAcc, hooks) (aId, n) = do
                (as', arch', hook) <- go $ nodeArchetype n
                let !nodes = Map.insert aId n {nodeArchetype = arch' <> nodeArchetype n} . AS.nodes $ archetypes esAcc
                return (as' V.++ acc, esAcc {archetypes = (archetypes esAcc) {AS.nodes = nodes}}, hooks >> hook)
           in foldlM go' (V.empty, es, return ()) $ Map.toList . AS.find cIds $ archetypes es
{-# INLINE queryDynM #-}

queryFilteredDyn :: Set ComponentID -> (Node Identity -> Bool) -> DynamicQuery a -> Entities Identity -> (Vector a, Entities Identity, AccessT Identity ())
queryFilteredDyn cIds f q = runIdentity . queryFilteredDynM cIds f q
{-# INLINE queryFilteredDyn #-}

-- | Map all matched entities.
queryFilteredDynM :: (Monad m) => Set ComponentID -> (Node m -> Bool) -> DynamicQueryT m a -> Entities m -> m (Vector a, Entities m, AccessT m ())
queryFilteredDynM cIds f q es =
  let go = runDynQueryT q
   in if Set.null cIds
        then do
          (as, _, hook) <- go A.empty {A.entities = Map.keysSet $ entities es}
          return (as, es, hook)
        else
          let go' (acc, esAcc, hooks) (aId, n) = do
                (as', arch', hook) <- go $ nodeArchetype n
                let !nodes = Map.insert aId n {nodeArchetype = arch' <> nodeArchetype n} . AS.nodes $ archetypes esAcc
                return (as' V.++ acc, esAcc {archetypes = (archetypes esAcc) {AS.nodes = nodes}}, hooks >> hook)
           in foldlM go' (V.empty, es, return ()) $ Map.toList . Map.filter f . AS.find cIds $ archetypes es
{-# INLINE queryFilteredDynM #-}

querySingleDyn :: (HasCallStack) => Set ComponentID -> DynamicQuery a -> Entities Identity -> (a, Entities Identity, AccessT Identity ())
querySingleDyn cIds q = runIdentity . querySingleDynM cIds q

-- | Map a single matched entity.
querySingleDynM :: (HasCallStack, Monad m) => Set ComponentID -> DynamicQueryT m a -> Entities m -> m (a, Entities m, AccessT m ())
querySingleDynM cIds q es = do
  res <- querySingleMaybeDynM cIds q es
  return $ case res of
    (Just a, es', hook) -> (a, es', hook)
    _ -> error "querySingleDyn: expected single matching entity"

querySingleMaybeDyn :: Set ComponentID -> DynamicQuery a -> Entities Identity -> (Maybe a, Entities Identity, AccessT Identity ())
querySingleMaybeDyn cIds q = runIdentity . querySingleMaybeDynM cIds q

-- | Map a single matched entity, or @Nothing@.
querySingleMaybeDynM :: (Monad m) => Set ComponentID -> DynamicQueryT m a -> Entities m -> m (Maybe a, Entities m, AccessT m ())
querySingleMaybeDynM cIds q es =
  if Set.null cIds
    then case Map.keys $ entities es of
      [eId] -> do
        res <- runDynQueryT q $ A.singleton eId
        return $ case res of
          (v, _, hook) | V.length v == 1 -> (Just (V.head v), es, hook)
          _ -> (Nothing, es, return ())
      _ -> pure (Nothing, es, return ())
    else case Map.toList $ AS.find cIds $ archetypes es of
      [(aId, n)] -> do
        res <- runDynQueryT q $ AS.nodeArchetype n
        return $ case res of
          (v, arch', hook)
            | V.length v == 1 ->
                let nodes = Map.insert aId n {nodeArchetype = arch' <> nodeArchetype n} . AS.nodes $ archetypes es
                 in (Just (V.head v), es {archetypes = (archetypes es) {AS.nodes = nodes}}, hook)
          _ -> (Nothing, es, return ())
      _ -> pure (Nothing, es, return ())
{-# INLINE querySingleMaybeDynM #-}

-- | Dynamic query filter.
data DynamicQueryFilter = DynamicQueryFilter
  { -- | `ComponentID`s to include.
    filterWith :: !(Set ComponentID),
    -- | `ComponentID`s to exclude.
    filterWithout :: !(Set ComponentID)
  }

instance Semigroup DynamicQueryFilter where
  DynamicQueryFilter withA withoutA <> DynamicQueryFilter withB withoutB =
    DynamicQueryFilter (withA <> withB) (withoutA <> withoutB)

instance Monoid DynamicQueryFilter where
  mempty = DynamicQueryFilter mempty mempty
