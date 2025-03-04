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
    ArrowDynamicQueryReader (..),
    ArrowDynamicQuery (..),

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
import Control.Arrow
import Control.Category
import Control.Monad.Identity
import Data.Either (partitionEithers)
import Data.Foldable
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Prelude hiding ((.))

-- | @since 9.0
type DynamicQuery = DynamicQueryT Identity

-- | Dynamic query for components by ID.
--
-- @since 9.0
newtype DynamicQueryT m i o
  = DynamicQuery
  { -- | Run a dynamic query with a list of inputs with length equal to the number of entities in the `Archetype`.
    -- This is an internal function that should typically not be used directly.
    --
    -- @since 9.0
    runDynQuery :: [i] -> Archetype -> m ([o], Archetype)
  }
  deriving (Functor)

-- | @since 9.0
instance (Monad m) => Applicative (DynamicQueryT m i) where
  {-# INLINE pure #-}
  pure a = DynamicQuery $ \is arch -> pure (replicate (length is) a, arch)
  {-# INLINE (<*>) #-}
  f <*> g = DynamicQuery $ \i arch -> do
    (as, arch') <- runDynQuery g i arch
    (fs, arch'') <- runDynQuery f i arch'
    return (zipWith ($) fs as, arch'')

-- | @since 9.0
instance (Monad m) => Category (DynamicQueryT m) where
  {-# INLINE id #-}
  id = DynamicQuery $ curry pure
  {-# INLINE (.) #-}
  f . g = DynamicQuery $ \i arch -> do
    (as, arch') <- runDynQuery g i arch
    runDynQuery f as arch'

-- | @since 9.0
instance (Monad m) => Arrow (DynamicQueryT m) where
  {-# INLINE arr #-}
  arr f = DynamicQuery $ \bs arch -> pure (fmap f bs, arch)
  {-# INLINE first #-}
  first f = DynamicQuery $ \bds arch -> do
    let !(bs, ds) = unzip bds
    (cs, arch') <- runDynQuery f bs arch
    return (zip cs ds, arch')

-- | @since 9.0
instance (Monad m) => ArrowChoice (DynamicQueryT m) where
  {-# INLINE left #-}
  left f = DynamicQuery $ \eds arch -> do
    let !(es', ds) = partitionEithers eds
    (cs, arch') <- runDynQuery f es' arch
    return (fmap Left cs ++ fmap Right ds, arch')

-- | @since 9.0
instance (Monad m) => ArrowDynamicQueryReader (DynamicQueryT m) where
  {-# INLINE entity #-}
  entity = fromDynReader entity
  {-# INLINE fetchDyn #-}
  fetchDyn = fromDynReader . fetchDyn
  {-# INLINE fetchMaybeDyn #-}
  fetchMaybeDyn = fromDynReader . fetchMaybeDyn

-- | @since 9.0
instance (Monad m) => ArrowDynamicQuery m (DynamicQueryT m) where
  {-# INLINE adjustDyn #-}
  adjustDyn f cId = DynamicQuery $ \is arch -> pure $ A.zipWith is f cId arch

  {-# INLINE adjustDyn_ #-}
  adjustDyn_ f cId = DynamicQuery $ \is arch -> pure (repeat (), A.zipWith_ is f cId arch)

  {-# INLINE adjustDynM #-}
  adjustDynM f cId = DynamicQuery $ \is arch -> A.zipWithM is f cId arch

  {-# INLINE setDyn #-}
  setDyn cId = DynamicQuery $ \is arch -> pure (is, A.insertAscList cId is arch)

-- | Convert a `DynamicQueryReaderT` to a `DynamicQueryT`.
--
-- @since 9.0
{-# INLINE fromDynReader #-}
fromDynReader :: (Monad m) => DynamicQueryReaderT m i o -> DynamicQueryT m i o
fromDynReader q = DynamicQuery $ \is arch -> do
  !os <- runDynQueryReader' q is arch
  return (os, arch)

-- | Convert a `DynamicQueryT` to a `DynamicQueryReaderT`.
--
-- @since 9.0
{-# INLINE toDynReader #-}
toDynReader :: (Functor m) => DynamicQueryT m i o -> DynamicQueryReaderT m i o
toDynReader q = DynamicQueryReader $ \is arch -> fst <$> runDynQuery q is arch

-- | Map all matched entities.
--
-- @since 9.0
{-# INLINE mapDyn #-}
mapDyn :: (Monad m) => Set ComponentID -> i -> DynamicQueryT m i a -> Entities -> m ([a], Entities)
mapDyn cIds i q es =
  let go = runDynQuery q (repeat i)
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
-- @since 9.0
{-# INLINE filterMapDyn #-}
filterMapDyn :: (Monad m) => Set ComponentID -> i -> (Node -> Bool) -> DynamicQueryT m i a -> Entities -> m ([a], Entities)
filterMapDyn cIds i f q es =
  let go = runDynQuery q (repeat i)
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
-- @since 9.0
mapSingleDyn :: (HasCallStack, Monad m) => Set ComponentID -> i -> DynamicQueryT m i a -> Entities -> m (a, Entities)
mapSingleDyn cIds i q es = do
  res <- mapSingleMaybeDyn cIds i q es
  return $ case res of
    (Just a, es') -> (a, es')
    _ -> error "mapSingleDyn: expected single matching entity"

-- | Map a single matched entity, or @Nothing@.
--
-- @since 9.0
{-# INLINE mapSingleMaybeDyn #-}
mapSingleMaybeDyn :: (Monad m) => Set ComponentID -> i -> DynamicQueryT m i a -> Entities -> m (Maybe a, Entities)
mapSingleMaybeDyn cIds i q es =
  if Set.null cIds
    then case Map.keys $ entities es of
      [eId] -> do
        res <- runDynQuery q [i] $ A.singleton eId
        return $ case res of
          ([a], _) -> (Just a, es)
          _ -> (Nothing, es)
      _ -> pure (Nothing, es)
    else case Map.toList $ AS.find cIds $ archetypes es of
      [(aId, n)] -> do
        res <- runDynQuery q [i] (AS.nodeArchetype n)
        return $ case res of
          ([a], arch') ->
            let nodes = Map.insert aId n {nodeArchetype = arch'} . AS.nodes $ archetypes es
             in (Just a, es {archetypes = (archetypes es) {AS.nodes = nodes}})
          _ -> (Nothing, es)
      _ -> pure (Nothing, es)
