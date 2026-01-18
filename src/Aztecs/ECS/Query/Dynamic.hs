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
    allDyn,
    allDynM,
    filterDyn,
    filterDynM,
    singleDyn,
    singleDynM,
    singleMaybeDyn,
    singleMaybeDynM,
    mapDyn,
    mapDynM,
    filterMapDyn,
    filterMapDynM,
    mapSingleDyn,
    mapSingleDynM,
    mapSingleMaybeDyn,
    mapSingleMaybeDynM,

    -- * Dynamic query filters
    DynamicQueryFilter (..),
  )
where

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
newtype DynamicQueryT f a
  = DynamicQuery
  { -- | Run a dynamic query.
    --
    -- @since 0.10
    runDynQueryT :: Archetype -> f (Vector a, Archetype)
  }
  deriving (Functor)

instance (Applicative f) => Applicative (DynamicQueryT f) where
  pure a = DynamicQuery $ \arch -> pure (V.replicate (length $ A.entities arch) a, arch)
  {-# INLINE pure #-}

  f <*> g = DynamicQuery $ \arch -> do
    x <- runDynQueryT g arch
    y <- runDynQueryT f arch
    return $
      let (as, arch') = x
          (bs, arch'') = y
       in (V.zipWith ($) bs as, arch' <> arch'')
  {-# INLINE (<*>) #-}

instance (Monad f) => DynamicQueryF f (DynamicQueryT f) where
  entity = DynamicQuery $ \arch -> pure (V.fromList . Set.toList $ A.entities arch, arch)
  {-# INLINE entity #-}

  fetchDyn cId = DynamicQuery $ \arch -> pure (A.lookupComponentsAsc cId arch, arch)
  {-# INLINE fetchDyn #-}

  fetchMaybeDyn cId = DynamicQuery $ \arch -> case A.lookupComponentsAscMaybe cId arch of
    Just as -> pure (V.map Just as, arch)
    Nothing -> pure (V.replicate (length $ A.entities arch) Nothing, arch)
  {-# INLINE fetchMaybeDyn #-}

  adjustDyn f cId q =
    DynamicQuery (fmap (\(bs, arch') -> A.zipWith bs f cId arch') . runDynQueryT q)
  {-# INLINE adjustDyn #-}

  adjustDyn_ f cId q = DynamicQuery $ \arch ->
    fmap (\(bs, arch') -> (V.map (const ()) bs, A.zipWith_ bs f cId arch')) (runDynQueryT q arch)
  {-# INLINE adjustDyn_ #-}

  adjustDynM f cId q = DynamicQuery $ \arch -> do
    (bs, arch') <- runDynQueryT q arch
    A.zipWithM bs f cId arch'
  {-# INLINE adjustDynM #-}

  setDyn cId q =
    DynamicQuery (fmap (\(bs, arch') -> (bs, A.insertAscVector cId bs arch')) . runDynQueryT q)
  {-# INLINE setDyn #-}

runDynQuery :: DynamicQuery a -> Archetype -> (Vector a, Archetype)
runDynQuery q = runIdentity . runDynQueryT q

-- | Match all entities.
allDyn :: Set ComponentID -> DynamicQuery a -> Entities -> (Vector a)
allDyn cIds q es = runIdentity $ allDynM cIds q es

-- | Match all entities.
allDynM :: (Monad m) => Set ComponentID -> DynamicQueryT m a -> Entities -> m (Vector a)
allDynM cIds q es =
  if Set.null cIds
    then fst <$> runDynQueryT q A.empty {A.entities = Map.keysSet $ entities es}
    else do
      let go n = fst <$> runDynQueryT q (AS.nodeArchetype n)
      results <- mapM go . Map.elems $ AS.find cIds $ archetypes es
      return $ V.concat results

filterDyn :: Set ComponentID -> DynamicQuery a -> (Node -> Bool) -> Entities -> (Vector a)
filterDyn cIds q f = runIdentity . filterDynM cIds f q

-- | Match all entities with a filter.
filterDynM :: (Monad m) => Set ComponentID -> (Node -> Bool) -> DynamicQueryT m a -> Entities -> m (Vector a)
filterDynM cIds f q es =
  if Set.null cIds
    then fst <$> runDynQueryT q A.empty {A.entities = Map.keysSet $ entities es}
    else do
      let go n = fst <$> runDynQueryT q (AS.nodeArchetype n)
      results <- mapM go . Map.elems . Map.filter f $ AS.find cIds $ archetypes es
      return $ V.concat results

-- | Match a single entity.
singleDyn :: (HasCallStack) => Set ComponentID -> DynamicQuery a -> Entities -> a
singleDyn cIds q = runIdentity . singleDynM cIds q

-- | Match a single entity.
singleDynM :: (HasCallStack, Monad m) => Set ComponentID -> DynamicQueryT m a -> Entities -> m a
singleDynM cIds q es = do
  res <- singleMaybeDynM cIds q es
  case res of
    Just a -> return a
    _ -> error "singleDyn: expected a single entity"

-- | Match a single entity, or `Nothing`.
singleMaybeDyn :: Set ComponentID -> DynamicQuery a -> Entities -> Maybe a
singleMaybeDyn cIds q = runIdentity . singleMaybeDynM cIds q

-- | Match a single entity, or `Nothing`.
singleMaybeDynM :: (Monad m) => Set ComponentID -> DynamicQueryT m a -> Entities -> m (Maybe a)
singleMaybeDynM cIds q es =
  if Set.null cIds
    then case Map.keys $ entities es of
      [eId] -> do
        (v, _) <- runDynQueryT q $ A.singleton eId
        return $ if V.length v == 1 then Just (V.head v) else Nothing
      _ -> return Nothing
    else case Map.elems $ AS.find cIds $ archetypes es of
      [n] -> do
        (v, _) <- runDynQueryT q $ AS.nodeArchetype n
        return $ if V.length v == 1 then Just (V.head v) else Nothing
      _ -> return Nothing

-- | Map all matched entities.
mapDyn :: Set ComponentID -> DynamicQuery a -> Entities -> (Vector a, Entities)
mapDyn cIds q = runIdentity . mapDynM cIds q
{-# INLINE mapDyn #-}

-- | Map all matched entities.
mapDynM :: (Monad m) => Set ComponentID -> DynamicQueryT m a -> Entities -> m (Vector a, Entities)
mapDynM cIds q es =
  let go = runDynQueryT q
   in if Set.null cIds
        then do
          (as, _) <- go A.empty {A.entities = Map.keysSet $ entities es}
          return (as, es)
        else
          let go' (acc, esAcc) (aId, n) = do
                (as', arch') <- go $ nodeArchetype n
                let !nodes = Map.insert aId n {nodeArchetype = arch' <> nodeArchetype n} . AS.nodes $ archetypes esAcc
                return (as' V.++ acc, esAcc {archetypes = (archetypes esAcc) {AS.nodes = nodes}})
           in foldlM go' (V.empty, es) $ Map.toList . AS.find cIds $ archetypes es
{-# INLINE mapDynM #-}

filterMapDyn :: Set ComponentID -> (Node -> Bool) -> DynamicQuery a -> Entities -> (Vector a, Entities)
filterMapDyn cIds f q = runIdentity . filterMapDynM cIds f q
{-# INLINE filterMapDyn #-}

-- | Map all matched entities.
filterMapDynM :: (Monad m) => Set ComponentID -> (Node -> Bool) -> DynamicQueryT m a -> Entities -> m (Vector a, Entities)
filterMapDynM cIds f q es =
  let go = runDynQueryT q
   in if Set.null cIds
        then do
          (as, _) <- go A.empty {A.entities = Map.keysSet $ entities es}
          return (as, es)
        else
          let go' (acc, esAcc) (aId, n) = do
                (as', arch') <- go $ nodeArchetype n
                let !nodes = Map.insert aId n {nodeArchetype = arch' <> nodeArchetype n} . AS.nodes $ archetypes esAcc
                return (as' V.++ acc, esAcc {archetypes = (archetypes esAcc) {AS.nodes = nodes}})
           in foldlM go' (V.empty, es) $ Map.toList . Map.filter f . AS.find cIds $ archetypes es
{-# INLINE filterMapDynM #-}

mapSingleDyn :: (HasCallStack) => Set ComponentID -> DynamicQuery a -> Entities -> (a, Entities)
mapSingleDyn cIds q = runIdentity . mapSingleDynM cIds q

-- | Map a single matched entity.
mapSingleDynM :: (HasCallStack, Monad m) => Set ComponentID -> DynamicQueryT m a -> Entities -> m (a, Entities)
mapSingleDynM cIds q es = do
  res <- mapSingleMaybeDynM cIds q es
  return $ case res of
    (Just a, es') -> (a, es')
    _ -> error "mapSingleDyn: expected single matching entity"

mapSingleMaybeDyn :: Set ComponentID -> DynamicQuery a -> Entities -> (Maybe a, Entities)
mapSingleMaybeDyn cIds q = runIdentity . mapSingleMaybeDynM cIds q

-- | Map a single matched entity, or @Nothing@.
mapSingleMaybeDynM :: (Monad m) => Set ComponentID -> DynamicQueryT m a -> Entities -> m (Maybe a, Entities)
mapSingleMaybeDynM cIds q es =
  if Set.null cIds
    then case Map.keys $ entities es of
      [eId] -> do
        res <- runDynQueryT q $ A.singleton eId
        return $ case res of
          (v, _) | V.length v == 1 -> (Just (V.head v), es)
          _ -> (Nothing, es)
      _ -> pure (Nothing, es)
    else case Map.toList $ AS.find cIds $ archetypes es of
      [(aId, n)] -> do
        res <- runDynQueryT q $ AS.nodeArchetype n
        return $ case res of
          (v, arch')
            | V.length v == 1 ->
                let nodes = Map.insert aId n {nodeArchetype = arch' <> nodeArchetype n} . AS.nodes $ archetypes es
                 in (Just (V.head v), es {archetypes = (archetypes es) {AS.nodes = nodes}})
          _ -> (Nothing, es)
      _ -> pure (Nothing, es)
{-# INLINE mapSingleMaybeDynM #-}

-- | Dynamic query filter.
data DynamicQueryFilter = DynamicQueryFilter
  { -- | `ComponentID`s to include.
    --
    -- @since 0.9
    filterWith :: !(Set ComponentID),
    -- | `ComponentID`s to exclude.
    --
    -- @since 0.9
    filterWithout :: !(Set ComponentID)
  }

instance Semigroup DynamicQueryFilter where
  DynamicQueryFilter withA withoutA <> DynamicQueryFilter withB withoutB =
    DynamicQueryFilter (withA <> withB) (withoutA <> withoutB)

instance Monoid DynamicQueryFilter where
  mempty = DynamicQueryFilter mempty mempty
