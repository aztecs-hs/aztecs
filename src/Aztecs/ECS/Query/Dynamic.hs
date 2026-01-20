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
    DynamicQuery (..),
    DynamicQueryF (..),

    -- ** Running
    readQueryDyn,
    readQueryFilteredDyn,
    readQuerySingleDyn,
    readQuerySingleMaybeDyn,
    queryDyn,
    queryFilteredDyn,
    querySingleDyn,
    querySingleMaybeDyn,

    -- * Dynamic query filters
    DynamicQueryFilter (..),
  )
where

import Aztecs.ECS.Access.Internal (Access)
import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic.Class
import Aztecs.ECS.World.Archetype (Archetype)
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Entities (Entities (..))
import Aztecs.ECS.World.Storage.Dynamic
import Data.Foldable
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Stack

-- | Dynamic query for components by ID.
newtype DynamicQuery m a
  = DynamicQuery
  { -- | Run a dynamic query.
    --
    -- @since 0.10
    runDynQuery :: Archetype m -> m (Vector a, Archetype m, Access m ())
  }
  deriving (Functor)

instance (Monad m) => Applicative (DynamicQuery m) where
  pure a = DynamicQuery $ \arch -> pure (V.replicate (length $ A.entities arch) a, arch, return ())
  {-# INLINE pure #-}

  f <*> g = DynamicQuery $ \arch -> do
    x <- runDynQuery g arch
    y <- runDynQuery f arch
    return $
      let (as, arch', hook1) = x
          (bs, arch'', hook2) = y
       in (V.zipWith ($) bs as, arch' <> arch'', hook1 >> hook2)
  {-# INLINE (<*>) #-}

instance (Monad m) => DynamicQueryF m (DynamicQuery m) where
  entity = DynamicQuery $ \arch -> pure (V.fromList . Set.toList $ A.entities arch, arch, return ())
  {-# INLINE entity #-}

  fetchDyn cId = DynamicQuery $ \arch -> pure (A.lookupComponentsAsc cId arch, arch, return ())
  {-# INLINE fetchDyn #-}

  fetchMaybeDyn cId = DynamicQuery $ \arch -> case A.lookupComponentsAscMaybe cId arch of
    Just as -> pure (V.map Just as, arch, return ())
    Nothing -> pure (V.replicate (length $ A.entities arch) Nothing, arch, return ())
  {-# INLINE fetchMaybeDyn #-}

  mapDyn f cId = DynamicQuery $ \arch -> do
    let (cs, arch', hook) = A.zipWith (V.replicate (length $ A.entities arch) ()) (const f) cId arch
    return (cs, arch', hook)
  {-# INLINE mapDyn #-}

  mapDyn_ f cId = DynamicQuery $ \arch -> do
    let (arch', hook) = A.zipWith_ (V.replicate (length $ A.entities arch) ()) (const f) cId arch
    return (V.replicate (length $ A.entities arch) (), arch', hook)
  {-# INLINE mapDyn_ #-}

  mapDynM f cId = DynamicQuery $ \arch -> do
    (cs, arch', hook) <- A.zipWithM (V.replicate (length $ A.entities arch) ()) (const f) cId arch
    return (cs, arch', hook)
  {-# INLINE mapDynM #-}

  mapDynWith f cId q = DynamicQuery $ \arch -> do
    (as, arch', hook1) <- runDynQuery q arch
    let (cs, arch'', hook2) = A.zipWith as f cId arch'
    return (cs, arch'', hook1 >> hook2)
  {-# INLINE mapDynWith #-}

  mapDynWith_ f cId q = DynamicQuery $ \arch -> do
    (as, arch', hook1) <- runDynQuery q arch
    let (arch'', hook2) = A.zipWith_ as f cId arch'
    return (V.map (const ()) as, arch'', hook1 >> hook2)
  {-# INLINE mapDynWith_ #-}

  mapDynWithM f cId q = DynamicQuery $ \arch -> do
    (as, arch', hook1) <- runDynQuery q arch
    (cs, arch'', hook2) <- A.zipWithM as f cId arch'
    return (cs, arch'', hook1 >> hook2)
  {-# INLINE mapDynWithM #-}

  mapDynWithAccum f cId q = DynamicQuery $ \arch -> do
    (bs, arch', hook1) <- runDynQuery q arch
    let (pairs, arch'', hook2) = A.zipWithAccum bs f cId arch'
    return (pairs, arch'', hook1 >> hook2)
  {-# INLINE mapDynWithAccum #-}

  mapDynWithAccumM f cId q = DynamicQuery $ \arch -> do
    (bs, arch', hook1) <- runDynQuery q arch
    (pairs, arch'', hook2) <- A.zipWithAccumM bs f cId arch'
    return (pairs, arch'', hook1 >> hook2)
  {-# INLINE mapDynWithAccumM #-}

  untracked q = DynamicQuery $ \arch -> do
    (as, arch', _hooks) <- runDynQuery q arch
    return (as, arch', return ())
  {-# INLINE untracked #-}

  filter q p = DynamicQuery $ \arch -> do
    (as, _, _) <- runDynQuery q arch
    let eIds = V.fromList . Set.toList $ A.entities arch
        mask = V.map p as
        (filteredEIds, indices) = V.unzip . V.imapMaybe (\i (e, m) -> if m then Just (e, i) else Nothing) $ V.zip eIds mask
        filteredArch = filterArchetype indices arch
    (filteredAs, filteredArch', hook) <- runDynQuery q filteredArch {A.entities = Set.fromList $ V.toList filteredEIds}
    let resultAs = unfilterVector indices as filteredAs
        resultArch = unfilterArchetype indices arch filteredArch'
    return (resultAs, resultArch, hook)
    where
      filterArchetype indices arch =
        arch {A.storages = IntMap.map (filterStorage indices) $ A.storages arch}
      filterStorage indices s =
        let allVec = toAscVectorDyn s
            filteredVec = V.map (allVec V.!) indices
         in fromAscVectorDyn filteredVec s
      unfilterVector indices original filtered =
        V.accum (\_ new -> new) original (V.toList $ V.zip indices filtered)
      unfilterArchetype indices original filtered =
        original {A.storages = IntMap.mapWithKey go $ A.storages original}
        where
          go cId origStorage = case IntMap.lookup cId (A.storages filtered) of
            Just filteredStorage ->
              let origVec = toAscVectorDyn origStorage
                  filteredVec = toAscVectorDyn filteredStorage
                  mergedVec = V.accum (\_ new -> new) origVec (V.toList $ V.zip indices filteredVec)
               in fromAscVectorDyn mergedVec origStorage
            Nothing -> origStorage
  {-# INLINE filter #-}

-- | Match all entities.
readQueryDyn :: (Monad m) => Set ComponentID -> DynamicQuery m a -> Entities m -> m (Vector a)
readQueryDyn cIds q es =
  if Set.null cIds
    then (\(a, _, _) -> a) <$> runDynQuery q A.empty {A.entities = Map.keysSet $ entities es}
    else do
      let go n = (\(a, _, _) -> a) <$> runDynQuery q (AS.nodeArchetype n)
      results <- mapM go . Map.elems $ AS.find cIds $ archetypes es
      return $ V.concat results

-- | Match all entities with a filter.
readQueryFilteredDyn :: (Monad m) => Set ComponentID -> (Node m -> Bool) -> DynamicQuery m a -> Entities m -> m (Vector a)
readQueryFilteredDyn cIds f q es =
  if Set.null cIds
    then (\(a, _, _) -> a) <$> runDynQuery q A.empty {A.entities = Map.keysSet $ entities es}
    else do
      let go n = (\(a, _, _) -> a) <$> runDynQuery q (AS.nodeArchetype n)
      results <- mapM go . Map.elems . Map.filter f $ AS.find cIds $ archetypes es
      return $ V.concat results

-- | Match a single entity.
readQuerySingleDyn :: (HasCallStack, Monad m) => Set ComponentID -> DynamicQuery m a -> Entities m -> m a
readQuerySingleDyn cIds q es = do
  res <- readQuerySingleMaybeDyn cIds q es
  case res of
    Just a -> return a
    _ -> error "readQuerySingleDyn: expected a single entity"

-- | Match a single entity, or `Nothing`.
readQuerySingleMaybeDyn :: (Monad m) => Set ComponentID -> DynamicQuery m a -> Entities m -> m (Maybe a)
readQuerySingleMaybeDyn cIds q es =
  if Set.null cIds
    then case Map.keys $ entities es of
      [eId] -> do
        (v, _, _) <- runDynQuery q $ A.singleton eId
        return $ if V.length v == 1 then Just (V.head v) else Nothing
      _ -> return Nothing
    else case Map.elems $ AS.find cIds $ archetypes es of
      [n] -> do
        (v, _, _) <- runDynQuery q $ AS.nodeArchetype n
        return $ if V.length v == 1 then Just (V.head v) else Nothing
      _ -> return Nothing

-- | Map all matched entities.
queryDyn :: (Monad m) => Set ComponentID -> DynamicQuery m a -> Entities m -> m (Vector a, Entities m, Access m ())
queryDyn cIds q es =
  let go = runDynQuery q
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
{-# INLINE queryDyn #-}

-- | Map all matched entities.
queryFilteredDyn :: (Monad m) => Set ComponentID -> (Node m -> Bool) -> DynamicQuery m a -> Entities m -> m (Vector a, Entities m, Access m ())
queryFilteredDyn cIds f q es =
  let go = runDynQuery q
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
{-# INLINE queryFilteredDyn #-}

-- | Map a single matched entity.
querySingleDyn :: (HasCallStack, Monad m) => Set ComponentID -> DynamicQuery m a -> Entities m -> m (a, Entities m, Access m ())
querySingleDyn cIds q es = do
  res <- querySingleMaybeDyn cIds q es
  return $ case res of
    (Just a, es', hook) -> (a, es', hook)
    _ -> error "querySingleDyn: expected single matching entity"

-- | Map a single matched entity, or @Nothing@.
querySingleMaybeDyn :: (Monad m) => Set ComponentID -> DynamicQuery m a -> Entities m -> m (Maybe a, Entities m, Access m ())
querySingleMaybeDyn cIds q es =
  if Set.null cIds
    then case Map.keys $ entities es of
      [eId] -> do
        res <- runDynQuery q $ A.singleton eId
        return $ case res of
          (v, _, hook) | V.length v == 1 -> (Just (V.head v), es, hook)
          _ -> (Nothing, es, return ())
      _ -> pure (Nothing, es, return ())
    else case Map.toList $ AS.find cIds $ archetypes es of
      [(aId, n)] -> do
        res <- runDynQuery q $ AS.nodeArchetype n
        return $ case res of
          (v, arch', hook)
            | V.length v == 1 ->
                let nodes = Map.insert aId n {nodeArchetype = arch' <> nodeArchetype n} . AS.nodes $ archetypes es
                 in (Just (V.head v), es {archetypes = (archetypes es) {AS.nodes = nodes}}, hook)
          _ -> (Nothing, es, return ())
      _ -> pure (Nothing, es, return ())
{-# INLINE querySingleMaybeDyn #-}

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
