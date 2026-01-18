{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
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
    insertAscVector,
    zipWith,
    zipWith_,
    zipWithM,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import qualified Aztecs.ECS.World.Storage as S
import Aztecs.ECS.World.Storage.Dynamic
import qualified Aztecs.ECS.World.Storage.Dynamic as S
import Control.Monad.Writer
  ( MonadWriter (..),
    WriterT (..),
    runWriter,
  )
import Data.Dynamic
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics
import Prelude hiding (map, zipWith)

-- | Archetype of entities and components.
-- An archetype is guranteed to contain one of each stored component per entity.
data Archetype = Archetype
  { -- | Component storages.
    storages :: !(IntMap DynamicStorage),
    -- | Entities stored in this archetype.
    entities :: !(Set EntityID)
  }
  deriving (Show, Generic)

instance Semigroup Archetype where
  a <> b = Archetype {storages = storages a <> storages b, entities = entities a <> entities b}

instance Monoid Archetype where
  mempty = empty

-- | Empty archetype.
empty :: Archetype
empty = Archetype {storages = IntMap.empty, entities = Set.empty}

-- | Archetype with a single entity.
singleton :: EntityID -> Archetype
singleton e = Archetype {storages = IntMap.empty, entities = Set.singleton e}

-- | Lookup a component `Storage` by its `ComponentID`.
lookupStorage :: (Component a) => ComponentID -> Archetype -> Maybe (StorageT a)
lookupStorage cId w = do
  !dynS <- IntMap.lookup (unComponentId cId) $ storages w
  fromDynamic $ storageDyn dynS
{-# INLINE lookupStorage #-}

-- | Lookup a component by its `EntityID` and `ComponentID`.
lookupComponent :: (Component a) => EntityID -> ComponentID -> Archetype -> Maybe a
lookupComponent e cId w = lookupComponents cId w Map.!? e
{-# INLINE lookupComponent #-}

-- | Lookup all components by their `ComponentID`.
lookupComponents :: (Component a) => ComponentID -> Archetype -> Map EntityID a
lookupComponents cId arch = case lookupComponentsAscMaybe cId arch of
  Just as -> Map.fromAscList $ zip (Set.toList $ entities arch) (V.toList as)
  Nothing -> Map.empty
{-# INLINE lookupComponents #-}

-- | Lookup all components by their `ComponentID`, in ascending order by their `EntityID`.
lookupComponentsAsc :: (Component a) => ComponentID -> Archetype -> Vector a
lookupComponentsAsc cId = fromMaybe V.empty . lookupComponentsAscMaybe cId
{-# INLINE lookupComponentsAsc #-}

-- | Lookup all components by their `ComponentID`, in ascending order by their `EntityID`.
lookupComponentsAscMaybe :: forall a. (Component a) => ComponentID -> Archetype -> Maybe (Vector a)
lookupComponentsAscMaybe cId arch = S.toAscVector <$> lookupStorage @a cId arch
{-# INLINE lookupComponentsAscMaybe #-}

-- | Insert a component into the archetype.
-- This assumes the archetype contains one of each stored component per entity.
insertComponent ::
  forall a. (Component a) => EntityID -> ComponentID -> a -> Archetype -> Archetype
insertComponent e cId c arch =
  let !storage =
        S.fromAscVector @a @(StorageT a) . V.fromList . Map.elems . Map.insert e c $ lookupComponents cId arch
   in arch {storages = IntMap.insert (unComponentId cId) (dynStorage @a storage) (storages arch)}

-- | @True@ if this archetype contains an entity with the provided `ComponentID`.
member :: ComponentID -> Archetype -> Bool
member cId = IntMap.member (unComponentId cId) . storages

-- | Zip a vector of components with a function and a component storage.
zipWith ::
  forall a c. (Component c) => Vector a -> (a -> c -> c) -> ComponentID -> Archetype -> (Vector c, Archetype)
zipWith as f cId arch =
  let go maybeDyn = case maybeDyn of
        Just dyn -> case fromDynamic $ storageDyn dyn of
          Just s -> do
            let !(cs', s') = S.zipWith @c @(StorageT c) f as s
            tell cs'
            return $ Just $ dyn {storageDyn = toDyn s'}
          Nothing -> return maybeDyn
        Nothing -> return Nothing
      !(storages', cs) = runWriter $ IntMap.alterF go (unComponentId cId) $ storages arch
   in (cs, arch {storages = storages'})
{-# INLINE zipWith #-}

-- | Zip a vector of components with a monadic function and a component storage.
zipWithM ::
  forall m a c. (Monad m, Component c) => Vector a -> (a -> c -> m c) -> ComponentID -> Archetype -> m (Vector c, Archetype)
zipWithM as f cId arch = do
  let go maybeDyn = case maybeDyn of
        Just dyn -> case fromDynamic $ storageDyn dyn of
          Just s ->
            WriterT $
              fmap
                (\(cs, s') -> (Just dyn {storageDyn = toDyn s'}, cs))
                (S.zipWithM @c @(StorageT c) f as s)
          Nothing -> pure maybeDyn
        Nothing -> pure Nothing
  res <- runWriterT $ IntMap.alterF go (unComponentId cId) $ storages arch
  return (snd res, arch {storages = fst res})

-- | Zip a vector of components with a function and a component storage.
zipWith_ ::
  forall a c. (Component c) => Vector a -> (a -> c -> c) -> ComponentID -> Archetype -> Archetype
zipWith_ as f cId arch =
  let maybeStorage = case IntMap.lookup (unComponentId cId) $ storages arch of
        Just dyn -> case fromDynamic $ storageDyn dyn of
          Just s ->
            let !s' = S.zipWith_ @c @(StorageT c) f as s in Just $ dyn {storageDyn = toDyn s'}
          Nothing -> Nothing
        Nothing -> Nothing
   in (empty {storages = maybe IntMap.empty (IntMap.singleton (unComponentId cId)) maybeStorage})
{-# INLINE zipWith_ #-}

-- | Insert a vector of components into the archetype, sorted in ascending order by their `EntityID`.
insertAscVector :: forall a. (Component a) => ComponentID -> Vector a -> Archetype -> Archetype
insertAscVector cId as arch =
  let !storage = dynStorage @a $ S.fromAscVector @a @(StorageT a) as
   in arch {storages = IntMap.insert (unComponentId cId) storage $ storages arch}
{-# INLINE insertAscVector #-}

-- | Remove an entity from an archetype, returning its components.
remove :: EntityID -> Archetype -> (IntMap Dynamic, Archetype)
remove e arch =
  let go (dynAcc, archAcc) (cId, dynS) =
        let cs = Map.fromAscList . zip (Set.toList $ entities arch) . V.toList $ toAscVectorDyn dynS
            !(dynA, cs') = Map.updateLookupWithKey (\_ _ -> Nothing) e cs
            dynS' = S.fromAscVectorDyn (V.fromList $ Map.elems cs') dynS
            !dynAcc' = case dynA of
              Just d -> IntMap.insert cId d dynAcc
              Nothing -> dynAcc
         in (dynAcc', archAcc {storages = IntMap.insert cId dynS' $ storages archAcc})
      arch' = arch {entities = Set.delete e $ entities arch}
   in foldl' go (IntMap.empty, arch') . IntMap.toList $ storages arch'

-- | Remove an entity from an archetype, returning its component storages.
removeStorages :: EntityID -> Archetype -> (IntMap DynamicStorage, Archetype)
removeStorages e arch =
  let go (dynAcc, archAcc) (cId, dynS) =
        let cs = Map.fromAscList . zip (Set.toList $ entities arch) . V.toList $ toAscVectorDyn dynS
            !(dynA, cs') = Map.updateLookupWithKey (\_ _ -> Nothing) e cs
            dynS' = S.fromAscVectorDyn (V.fromList $ Map.elems cs') dynS
            !dynAcc' = case dynA of
              Just d -> IntMap.insert cId (S.singletonDyn d dynS') dynAcc
              Nothing -> dynAcc
         in (dynAcc', archAcc {storages = IntMap.insert cId dynS' $ storages archAcc})
      arch' = arch {entities = Set.delete e $ entities arch}
   in foldl' go (IntMap.empty, arch') . IntMap.toList $ storages arch'

-- | Insert a map of component storages and their `EntityID` into the archetype.
insertComponents :: EntityID -> IntMap Dynamic -> Archetype -> Archetype
insertComponents e cs arch =
  let f archAcc (itemCId, dyn) =
        let storages' = IntMap.adjust go itemCId (storages archAcc)
            es = Set.toList $ entities archAcc
            go s =
              let ecs = V.fromList . Map.elems . Map.insert e dyn . Map.fromAscList . zip es . V.toList $ toAscVectorDyn s
               in fromAscVectorDyn ecs s
         in archAcc {storages = storages', entities = Set.insert e $ entities archAcc}
   in foldl' f arch (IntMap.toList cs)
