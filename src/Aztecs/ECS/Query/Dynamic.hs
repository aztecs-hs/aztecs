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
    runQueryDyn,
    runQueryFilteredDyn,
    runQuerySingleDyn,
    runQuerySingleMaybeDyn,

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
import GHC.Stack

-- | Dynamic query for components by ID.
newtype DynamicQuery m a
  = DynamicQuery
  { -- | Run a dynamic query.
    --
    -- @since 0.10
    runDynQuery :: Archetype m -> m ([a], Archetype m, Access m ())
  }
  deriving (Functor)

instance (Monad m) => Applicative (DynamicQuery m) where
  pure a = DynamicQuery $ \arch -> pure (replicate (length $ A.entities arch) a, arch, return ())
  {-# INLINE pure #-}

  f <*> g = DynamicQuery $ \arch -> do
    x <- runDynQuery g arch
    y <- runDynQuery f arch
    return $
      let (as, arch', hook1) = x
          (bs, arch'', hook2) = y
       in (zipWith ($) bs as, arch' <> arch'', hook1 >> hook2)
  {-# INLINE (<*>) #-}

instance (Monad m) => DynamicQueryF m (DynamicQuery m) where
  entity = DynamicQuery $ \arch -> pure (Set.toList $ A.entities arch, arch, return ())
  {-# INLINE entity #-}

  queryDyn cId = DynamicQuery $ \arch -> pure (A.lookupComponentsAsc cId arch, arch, return ())
  {-# INLINE queryDyn #-}

  queryMaybeDyn cId = DynamicQuery $ \arch -> case A.lookupComponentsAscMaybe cId arch of
    Just as -> pure (map Just as, arch, return ())
    Nothing -> pure (replicate (length $ A.entities arch) Nothing, arch, return ())
  {-# INLINE queryMaybeDyn #-}

  queryMapDyn f cId = DynamicQuery $ \arch -> do
    let (cs, arch', hook) = A.zipWith (replicate (length $ A.entities arch) ()) (const f) cId arch
    return (cs, arch', hook)
  {-# INLINE queryMapDyn #-}

  queryMapDyn_ f cId = DynamicQuery $ \arch -> do
    let (arch', hook) = A.zipWith_ (replicate (length $ A.entities arch) ()) (const f) cId arch
    return (replicate (length $ A.entities arch) (), arch', hook)
  {-# INLINE queryMapDyn_ #-}

  queryMapDynM f cId = DynamicQuery $ \arch -> do
    (cs, arch', hook) <- A.zipWithM (replicate (length $ A.entities arch) ()) (const f) cId arch
    return (cs, arch', hook)
  {-# INLINE queryMapDynM #-}

  queryMapDynWith f cId q = DynamicQuery $ \arch -> do
    (as, arch', hook1) <- runDynQuery q arch
    let (cs, arch'', hook2) = A.zipWith as f cId arch'
    return (cs, arch'', hook1 >> hook2)
  {-# INLINE queryMapDynWith #-}

  queryMapDynWith_ f cId q = DynamicQuery $ \arch -> do
    (as, arch', hook1) <- runDynQuery q arch
    let (arch'', hook2) = A.zipWith_ as f cId arch'
    return (map (const ()) as, arch'', hook1 >> hook2)
  {-# INLINE queryMapDynWith_ #-}

  queryMapDynWithM f cId q = DynamicQuery $ \arch -> do
    (as, arch', hook1) <- runDynQuery q arch
    (cs, arch'', hook2) <- A.zipWithM as f cId arch'
    return (cs, arch'', hook1 >> hook2)
  {-# INLINE queryMapDynWithM #-}

  queryMapDynWithAccum f cId q = DynamicQuery $ \arch -> do
    (bs, arch', hook1) <- runDynQuery q arch
    let (pairs, arch'', hook2) = A.zipWithAccum bs f cId arch'
    return (pairs, arch'', hook1 >> hook2)
  {-# INLINE queryMapDynWithAccum #-}

  queryMapDynWithAccumM f cId q = DynamicQuery $ \arch -> do
    (bs, arch', hook1) <- runDynQuery q arch
    (pairs, arch'', hook2) <- A.zipWithAccumM bs f cId arch'
    return (pairs, arch'', hook1 >> hook2)
  {-# INLINE queryMapDynWithAccumM #-}

  queryUntracked q = DynamicQuery $ \arch -> do
    (as, arch', _hooks) <- runDynQuery q arch
    return (as, arch', return ())
  {-# INLINE queryUntracked #-}

  queryFilterMap p q = DynamicQuery $ \arch -> do
    (as, _, _) <- runDynQuery q arch
    let eIds = Set.toList $ A.entities arch
        mapped = map p as
        withIndices = zip3 eIds [0 ..] mapped
        filtered = [(e, i, b) | (e, i, Just b) <- withIndices]
        (filteredEIds, indices, filteredBs) = unzip3 filtered
        filteredArch = filterArchetype indices arch
    (_, filteredArch', hook) <- runDynQuery q filteredArch {A.entities = Set.fromList filteredEIds}
    let resultArch = unfilterArchetype indices arch filteredArch'
    return (filteredBs, resultArch, hook)
    where
      filterArchetype indices arch =
        arch {A.storages = IntMap.map (filterStorage indices) $ A.storages arch}
      filterStorage indices s =
        let allList = toAscListDyn s
            filteredList = map (allList !!) indices
         in fromAscListDyn filteredList s
      unfilterArchetype indices original filtered =
        original {A.storages = IntMap.mapWithKey go $ A.storages original}
        where
          go cId s = case IntMap.lookup cId (A.storages filtered) of
            Just filteredStorage ->
              let origList = toAscListDyn s
                  filteredList = toAscListDyn filteredStorage
                  updates = zip indices filteredList
                  mergedList = foldr (\(i, v) acc -> take i acc ++ [v] ++ drop (i + 1) acc) origList updates
               in fromAscListDyn mergedList s
            Nothing -> s
  {-# INLINE queryFilterMap #-}

-- | Match all entities.
readQueryDyn :: (Monad m) => Set ComponentID -> DynamicQuery m a -> Entities m -> m [a]
readQueryDyn cIds q es =
  if Set.null cIds
    then (\(a, _, _) -> a) <$> runDynQuery q A.empty {A.entities = Map.keysSet $ entities es}
    else do
      let go n = (\(a, _, _) -> a) <$> runDynQuery q (AS.nodeArchetype n)
      results <- mapM go . Map.elems $ AS.find cIds $ archetypes es
      return $ concat results

-- | Match all entities with a filter.
readQueryFilteredDyn :: (Monad m) => Set ComponentID -> (Node m -> Bool) -> DynamicQuery m a -> Entities m -> m [a]
readQueryFilteredDyn cIds f q es =
  if Set.null cIds
    then (\(a, _, _) -> a) <$> runDynQuery q A.empty {A.entities = Map.keysSet $ entities es}
    else do
      let go n = (\(a, _, _) -> a) <$> runDynQuery q (AS.nodeArchetype n)
      results <- mapM go . Map.elems . Map.filter f $ AS.find cIds $ archetypes es
      return $ concat results

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
        return $ case v of
          [x] -> Just x
          _ -> Nothing
      _ -> return Nothing
    else case Map.elems $ AS.find cIds $ archetypes es of
      [n] -> do
        (v, _, _) <- runDynQuery q $ AS.nodeArchetype n
        return $ case v of
          [x] -> Just x
          _ -> Nothing
      _ -> return Nothing

-- | Map all matched entities.
runQueryDyn :: (Monad m) => Set ComponentID -> DynamicQuery m a -> Entities m -> m ([a], Entities m, Access m ())
runQueryDyn cIds q es =
  let go = runDynQuery q
   in if Set.null cIds
        then do
          (as, _, hook) <- go A.empty {A.entities = Map.keysSet $ entities es}
          return (as, es, hook)
        else
          let go' (acc, esAcc, hooks) (aId, n) = do
                (as', arch', hook) <- go $ nodeArchetype n
                let !nodes = Map.insert aId n {nodeArchetype = arch' <> nodeArchetype n} . AS.nodes $ archetypes esAcc
                return (as' ++ acc, esAcc {archetypes = (archetypes esAcc) {AS.nodes = nodes}}, hooks >> hook)
           in foldlM go' ([], es, return ()) $ Map.toList . AS.find cIds $ archetypes es
{-# INLINE runQueryDyn #-}

-- | Map all matched entities.
runQueryFilteredDyn :: (Monad m) => Set ComponentID -> (Node m -> Bool) -> DynamicQuery m a -> Entities m -> m ([a], Entities m, Access m ())
runQueryFilteredDyn cIds f q es =
  let go = runDynQuery q
   in if Set.null cIds
        then do
          (as, _, hook) <- go A.empty {A.entities = Map.keysSet $ entities es}
          return (as, es, hook)
        else
          let go' (acc, esAcc, hooks) (aId, n) = do
                (as', arch', hook) <- go $ nodeArchetype n
                let !nodes = Map.insert aId n {nodeArchetype = arch' <> nodeArchetype n} . AS.nodes $ archetypes esAcc
                return (as' ++ acc, esAcc {archetypes = (archetypes esAcc) {AS.nodes = nodes}}, hooks >> hook)
           in foldlM go' ([], es, return ()) $ Map.toList . Map.filter f . AS.find cIds $ archetypes es
{-# INLINE runQueryFilteredDyn #-}

-- | Map a single matched entity.
runQuerySingleDyn :: (HasCallStack, Monad m) => Set ComponentID -> DynamicQuery m a -> Entities m -> m (a, Entities m, Access m ())
runQuerySingleDyn cIds q es = do
  res <- runQuerySingleMaybeDyn cIds q es
  return $ case res of
    (Just a, es', hook) -> (a, es', hook)
    _ -> error "querySingleDyn: expected single matching entity"

-- | Map a single matched entity, or @Nothing@.
runQuerySingleMaybeDyn :: (Monad m) => Set ComponentID -> DynamicQuery m a -> Entities m -> m (Maybe a, Entities m, Access m ())
runQuerySingleMaybeDyn cIds q es =
  if Set.null cIds
    then case Map.keys $ entities es of
      [eId] -> do
        res <- runDynQuery q $ A.singleton eId
        return $ case res of
          ([x], _, hook) -> (Just x, es, hook)
          _ -> (Nothing, es, return ())
      _ -> pure (Nothing, es, return ())
    else case Map.toList $ AS.find cIds $ archetypes es of
      [(aId, n)] -> do
        res <- runDynQuery q $ AS.nodeArchetype n
        return $ case res of
          ([x], arch', hook) ->
            let nodes = Map.insert aId n {nodeArchetype = arch' <> nodeArchetype n} . AS.nodes $ archetypes es
             in (Just x, es {archetypes = (archetypes es) {AS.nodes = nodes}}, hook)
          _ -> (Nothing, es, return ())
      _ -> pure (Nothing, es, return ())
{-# INLINE runQuerySingleMaybeDyn #-}

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
  {-# INLINE (<>) #-}

instance Monoid DynamicQueryFilter where
  mempty = DynamicQueryFilter mempty mempty
  {-# INLINE mempty #-}
