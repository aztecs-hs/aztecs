{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.ECS.World.Archetype
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Archetype
  ( Archetype (..),
    empty,
    singleton,
    lookupComponent,
    lookupComponents,
    lookupComponentsAsc,
    lookupComponentsAscMaybe,
    lookupStorage,
    member,
    remove,
    removeStorages,
    insertComponent,
    insertComponents,
    insertAscList,
    map,
    mapM,
    zipMap,
    zipMapM,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import qualified Aztecs.ECS.World.Storage as S
import Aztecs.ECS.World.Storage.Dynamic
import qualified Aztecs.ECS.World.Storage.Dynamic as S
import Control.DeepSeq
import Data.Dynamic
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import Prelude hiding (map, mapM, zipWith)

-- | Archetype of entities and components.
-- An archetype is guranteed to contain one of each stored component per entity.
--
-- @since 0.9
data Archetype = Archetype
  { -- | Component storages.
    --
    -- @since 0.9
    storages :: !(IntMap DynamicStorage),
    -- | Entities stored in this archetype.
    --
    -- @since 0.9
    entities :: !(Set EntityID)
  }
  deriving (Show, Generic)

instance Semigroup Archetype where
  a <> b = Archetype {storages = storages a <> storages b, entities = entities a <> entities b}

instance Monoid Archetype where
  mempty = empty

instance NFData Archetype where
  rnf = rnf . entities

-- | Empty archetype.
--
-- @since 0.9
empty :: Archetype
empty = Archetype {storages = IntMap.empty, entities = Set.empty}

-- | Archetype with a single entity.
--
-- @since 0.9
singleton :: EntityID -> Archetype
singleton e = Archetype {storages = IntMap.empty, entities = Set.singleton e}

-- | Lookup a component `Storage` by its `ComponentID`.
--
-- @since 0.9
{-# INLINE lookupStorage #-}
lookupStorage :: (Component a) => ComponentID -> Archetype -> Maybe (StorageT a)
lookupStorage cId w = do
  dynS <- IntMap.lookup (unComponentId cId) $ storages w
  fromDynamic $ storageDyn dynS

-- | Lookup a component by its `EntityID` and `ComponentID`.
--
-- @since 0.9
{-# INLINE lookupComponent #-}
lookupComponent :: (Component a) => EntityID -> ComponentID -> Archetype -> Maybe a
lookupComponent e cId w = lookupComponents cId w Map.!? e

-- | Lookup all components by their `ComponentID`.
--
-- @since 0.9
{-# INLINE lookupComponents #-}
lookupComponents :: (Component a) => ComponentID -> Archetype -> Map EntityID a
lookupComponents cId arch = case lookupComponentsAscMaybe cId arch of
  Just as -> Map.fromAscList $ zip (Set.toList $ entities arch) as
  Nothing -> Map.empty

-- | Lookup all components by their `ComponentID`, in ascending order by their `EntityID`.
--
-- @since 0.9
{-# INLINE lookupComponentsAsc #-}
lookupComponentsAsc :: (Component a) => ComponentID -> Archetype -> [a]
lookupComponentsAsc cId = fromMaybe [] . lookupComponentsAscMaybe cId

-- | Lookup all components by their `ComponentID`, in ascending order by their `EntityID`.
--
-- @since 0.9
{-# INLINE lookupComponentsAscMaybe #-}
lookupComponentsAscMaybe :: forall a. (Component a) => ComponentID -> Archetype -> Maybe [a]
lookupComponentsAscMaybe cId arch = S.toAscList <$> lookupStorage @a cId arch

-- | Insert a component into the archetype.
-- This assumes the archetype contains one of each stored component per entity.
--
-- @since 0.9
insertComponent ::
  forall a. (Component a) => EntityID -> ComponentID -> a -> Archetype -> Archetype
insertComponent e cId c arch =
  let !storage =
        S.fromAscList @a @(StorageT a) . Map.elems . Map.insert e c $ lookupComponents cId arch
   in arch {storages = IntMap.insert (unComponentId cId) (dynStorage @a storage) (storages arch)}

-- | @True@ if this archetype contains an entity with the provided `ComponentID`.
--
-- @since 0.9
{-# INLINE member #-}
member :: ComponentID -> Archetype -> Bool
member cId = IntMap.member (unComponentId cId) . storages

-- | Map a list of components with a function and a component storage.
--
-- @since 0.11
{-# INLINE map #-}
map ::
  forall a. (Component a) => (a -> a) -> ComponentID -> Archetype -> ([a], Archetype)
map f cId arch = case IntMap.lookup (unComponentId cId) $ storages arch of
  Just dyn -> case fromDynamic $ storageDyn dyn of
    Just s ->
      let (acs, s') = S.map @a @(StorageT a) f s
          s'' = dyn {storageDyn = toDyn s'}
       in (acs, mempty {storages = IntMap.singleton (unComponentId cId) s''})
    Nothing -> ([], mempty)
  Nothing -> ([], mempty)

-- | Map a list of components with a monadic function.
--
-- @since 0.11
{-# INLINE mapM #-}
mapM ::
  forall m a.
  (Monad m, Component a) =>
  (a -> m a) ->
  ComponentID ->
  Archetype ->
  m ([a], Archetype)
mapM f cId arch = case IntMap.lookup (unComponentId cId) $ storages arch of
  Just dyn -> case fromDynamic $ storageDyn dyn of
    Just s -> do
      (acs, s') <- S.mapM @a @(StorageT a) f s
      let s'' = dyn {storageDyn = toDyn s'}
      return (acs, mempty {storages = IntMap.singleton (unComponentId cId) s''})
    Nothing -> return ([], mempty)
  Nothing -> return ([], mempty)

-- | Zip a list of components with a function.
--
-- @since 0.9
{-# INLINE zipMap #-}
zipMap ::
  forall a b c.
  (Component c) =>
  [a] ->
  (a -> c -> (b, c)) ->
  ComponentID ->
  Archetype ->
  ([(b, c)], Archetype)
zipMap as f cId arch = case IntMap.lookup (unComponentId cId) $ storages arch of
  Just dyn -> case fromDynamic $ storageDyn dyn of
    Just s ->
      let (acs, s') = S.zipWith @c @(StorageT c) f as s
          s'' = dyn {storageDyn = toDyn s'}
       in (acs, mempty {storages = IntMap.singleton (unComponentId cId) s''})
    Nothing -> ([], mempty)
  Nothing -> ([], mempty)

-- | Zip a list of components with a monadic function .
--
-- @since 0.9
zipMapM ::
  forall m a b c.
  (Applicative m, Component c) =>
  [a] ->
  (a -> c -> m (b, c)) ->
  ComponentID ->
  Archetype ->
  m ([(b, c)], Archetype)
zipMapM as f cId arch = case IntMap.lookup (unComponentId cId) $ storages arch of
  Just dyn -> case fromDynamic $ storageDyn dyn of
    Just s -> do
      res <- S.zipWithM @c @(StorageT c) f as s
      return $
        let (acs, s') = res
            s'' = dyn {storageDyn = toDyn s'}
         in (acs, mempty {storages = IntMap.singleton (unComponentId cId) s''})
    Nothing -> pure ([], mempty)
  Nothing -> pure ([], mempty)

-- | Insert a list of components into the archetype, sorted in ascending order by their `EntityID`.
--
-- @since 0.9
{-# INLINE insertAscList #-}
insertAscList :: forall a. (Component a) => ComponentID -> [a] -> Archetype -> Archetype
insertAscList cId as arch =
  let !storage = dynStorage @a $ S.fromAscList @a @(StorageT a) as
   in arch {storages = IntMap.insert (unComponentId cId) storage $ storages arch}

-- | Remove an entity from an archetype, returning its components.
--
-- @since 0.9
remove :: EntityID -> Archetype -> (IntMap Dynamic, Archetype)
remove e arch =
  let go (dynAcc, archAcc) (cId, dynS) =
        let cs = Map.fromAscList . zip (Set.toList $ entities arch) $ toAscListDyn dynS
            !(dynA, cs') = Map.updateLookupWithKey (\_ _ -> Nothing) e cs
            dynS' = S.fromAscListDyn (Map.elems cs') dynS
            !dynAcc' = case dynA of
              Just d -> IntMap.insert cId d dynAcc
              Nothing -> dynAcc
         in (dynAcc', archAcc {storages = IntMap.insert cId dynS' $ storages archAcc})
      arch' = arch {entities = Set.delete e $ entities arch}
   in foldl' go (IntMap.empty, arch') . IntMap.toList $ storages arch'

-- | Remove an entity from an archetype, returning its component storages.
--
-- @since 0.9
removeStorages :: EntityID -> Archetype -> (IntMap DynamicStorage, Archetype)
removeStorages e arch =
  let go (dynAcc, archAcc) (cId, dynS) =
        let cs = Map.fromAscList . zip (Set.toList $ entities arch) $ toAscListDyn dynS
            !(dynA, cs') = Map.updateLookupWithKey (\_ _ -> Nothing) e cs
            dynS' = S.fromAscListDyn (Map.elems cs') dynS
            !dynAcc' = case dynA of
              Just d -> IntMap.insert cId (S.singletonDyn d dynS') dynAcc
              Nothing -> dynAcc
         in (dynAcc', archAcc {storages = IntMap.insert cId dynS' $ storages archAcc})
      arch' = arch {entities = Set.delete e $ entities arch}
   in foldl' go (IntMap.empty, arch') . IntMap.toList $ storages arch'

-- | Insert a map of component storages and their `EntityID` into the archetype.
--
-- @since 0.9
insertComponents :: EntityID -> IntMap Dynamic -> Archetype -> Archetype
insertComponents e cs arch =
  let f archAcc (itemCId, dyn) =
        let storages' = IntMap.adjust go itemCId (storages archAcc)
            es = Set.toList $ entities archAcc
            go s =
              let ecs = Map.elems . Map.insert e dyn . Map.fromAscList . zip es $ toAscListDyn s
               in fromAscListDyn ecs s
         in archAcc {storages = storages', entities = Set.insert e $ entities archAcc}
   in foldl' f arch (IntMap.toList cs)
