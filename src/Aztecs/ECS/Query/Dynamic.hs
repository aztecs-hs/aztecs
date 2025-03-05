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
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic.Class
import Aztecs.ECS.Query.Dynamic.Reader
import Aztecs.ECS.World.Archetype (Archetype)
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Category
import Control.Monad.Identity
import Data.Foldable
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Prelude hiding ((.))

type DynamicQuery = DynamicQueryT Identity

-- | Dynamic query for components by ID.
--
-- @since 0.10
newtype DynamicQueryT m a
  = DynamicQuery
  { -- | Run a dynamic query.
    --
    -- @since 0.10
    runDynQuery :: Archetype -> m ([a], Archetype)
  }
  deriving (Functor)

-- | @since 0.10
instance (Monad m) => Applicative (DynamicQueryT m) where
  {-# INLINE pure #-}
  pure a = DynamicQuery $ \arch -> pure (replicate (length $ A.entities arch) a, arch)

  {-# INLINE (<*>) #-}
  f <*> g = DynamicQuery $ \arch -> do
    (as, arch') <- runDynQuery g arch
    (fs, arch'') <- runDynQuery f arch'
    return (zipWith ($) fs as, arch'')

-- | @since 0.10
instance DynamicQueryReaderF DynamicQuery where
  {-# INLINE entity #-}
  entity = fromDynReader entity

  {-# INLINE fetchDyn #-}
  fetchDyn = fromDynReader . fetchDyn

  {-# INLINE fetchMaybeDyn #-}
  fetchMaybeDyn = fromDynReader . fetchMaybeDyn

-- | @since 0.10
instance (Monad m) => DynamicQueryF m (DynamicQueryT m) where
  {-# INLINE adjustDyn #-}
  adjustDyn f cId q = DynamicQuery $ \arch -> do
    (bs, arch') <- runDynQuery q arch
    return $ A.zipWith bs f cId arch'

  {-# INLINE adjustDyn_ #-}
  adjustDyn_ f cId q = DynamicQuery $ \arch -> do
    (bs, arch') <- runDynQuery q arch
    return (map (const ()) bs, A.zipWith_ bs f cId arch')

  {-# INLINE adjustDynM #-}
  adjustDynM f cId q = DynamicQuery $ \arch -> do
    (bs, arch') <- runDynQuery q arch
    A.zipWithM bs f cId arch'

  {-# INLINE setDyn #-}
  setDyn cId q = DynamicQuery $ \arch -> do
    (bs, arch') <- runDynQuery q arch
    return (bs, A.insertAscList cId bs arch')

-- | Convert a `DynamicQueryReaderT` to a `DynamicQueryT`.
--
-- @since 0.10
{-# INLINE fromDynReader #-}
fromDynReader :: (Monad m) => DynamicQueryReader a -> DynamicQueryT m a
fromDynReader q = DynamicQuery $ \arch -> do
  let !os = runDynQueryReader q arch
  return (os, arch)

-- | Convert a `DynamicQueryT` to a `DynamicQueryReaderT`.
--
-- @since 0.10
{-# INLINE toDynReader #-}
toDynReader :: DynamicQuery a -> DynamicQueryReader a
toDynReader q = DynamicQueryReader $ \arch -> fst $ runIdentity $ runDynQuery q arch

-- | Map all matched entities.
--
-- @since 0.10
{-# INLINE mapDyn #-}
mapDyn :: (Monad m) => Set ComponentID -> DynamicQueryT m a -> Entities -> m ([a], Entities)
mapDyn cIds q es =
  let go = runDynQuery q
   in if Set.null cIds
        then do
          (as, _) <- go A.empty {A.entities = Map.keysSet $ entities es}
          return (as, es)
        else
          let go' (acc, esAcc) (aId, n) = do
                (as', arch') <- go $ nodeArchetype n
                let !nodes = Map.insert aId n {nodeArchetype = arch'} . AS.nodes $ archetypes esAcc
                return (as' ++ acc, esAcc {archetypes = (archetypes esAcc) {AS.nodes = nodes}})
           in foldlM go' ([], es) $ Map.toList . AS.find cIds $ archetypes es

-- | Map all matched entities.
--
-- @since 0.10
{-# INLINE filterMapDyn #-}
filterMapDyn :: (Monad m) => Set ComponentID -> (Node -> Bool) -> DynamicQueryT m a -> Entities -> m ([a], Entities)
filterMapDyn cIds f q es =
  let go = runDynQuery q
   in if Set.null cIds
        then do
          (as, _) <- go A.empty {A.entities = Map.keysSet $ entities es}
          return (as, es)
        else
          let go' (acc, esAcc) (aId, n) = do
                (as', arch') <- go $ nodeArchetype n
                let !nodes = Map.insert aId n {nodeArchetype = arch'} . AS.nodes $ archetypes esAcc
                return (as' ++ acc, esAcc {archetypes = (archetypes esAcc) {AS.nodes = nodes}})
           in foldlM go' ([], es) $ Map.toList . Map.filter f . AS.find cIds $ archetypes es

-- | Map a single matched entity.
--
-- @since 0.10
mapSingleDyn :: (HasCallStack, Monad m) => Set ComponentID -> DynamicQueryT m a -> Entities -> m (a, Entities)
mapSingleDyn cIds q es = do
  res <- mapSingleMaybeDyn cIds q es
  return $ case res of
    (Just a, es') -> (a, es')
    _ -> error "mapSingleDyn: expected single matching entity"

-- | Map a single matched entity, or @Nothing@.
--
-- @since 0.10
{-# INLINE mapSingleMaybeDyn #-}
mapSingleMaybeDyn :: (Monad m) => Set ComponentID -> DynamicQueryT m a -> Entities -> m (Maybe a, Entities)
mapSingleMaybeDyn cIds q es =
  if Set.null cIds
    then case Map.keys $ entities es of
      [eId] -> do
        res <- runDynQuery q $ A.singleton eId
        return $ case res of
          ([a], _) -> (Just a, es)
          _ -> (Nothing, es)
      _ -> pure (Nothing, es)
    else case Map.toList $ AS.find cIds $ archetypes es of
      [(aId, n)] -> do
        res <- runDynQuery q $ AS.nodeArchetype n
        return $ case res of
          ([a], arch') ->
            let nodes = Map.insert aId n {nodeArchetype = arch'} . AS.nodes $ archetypes es
             in (Just a, es {archetypes = (archetypes es) {AS.nodes = nodes}})
          _ -> (Nothing, es)
      _ -> pure (Nothing, es)
