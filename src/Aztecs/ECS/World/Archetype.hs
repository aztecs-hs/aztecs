{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
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

import Aztecs.ECS.Access.Internal (AccessT)
import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.World.Archetype.Internal (Archetype (..), empty)
import qualified Aztecs.ECS.World.Storage as S
import Aztecs.ECS.World.Storage.Dynamic
import qualified Aztecs.ECS.World.Storage.Dynamic as S
import Control.Monad.Writer
import Data.Dynamic
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (map, zipWith)

-- | Archetype with a single entity.
singleton :: EntityID -> Archetype m
singleton e = Archetype {storages = IntMap.empty, entities = Set.singleton e}

-- | Lookup a component `Storage` by its `ComponentID`.
lookupStorage :: (Component m a) => ComponentID -> Archetype m -> Maybe (StorageT a)
lookupStorage cId w = do
  !dynS <- IntMap.lookup (unComponentId cId) $ storages w
  fromDynamic $ storageDyn dynS
{-# INLINE lookupStorage #-}

-- | Lookup a component by its `EntityID` and `ComponentID`.
lookupComponent :: forall m a. (Component m a) => EntityID -> ComponentID -> Archetype m -> Maybe a
lookupComponent e cId w = lookupComponents cId w Map.!? e
{-# INLINE lookupComponent #-}

-- | Lookup all components by their `ComponentID`.
lookupComponents :: forall m a. (Component m a) => ComponentID -> Archetype m -> Map EntityID a
lookupComponents cId arch = case lookupComponentsAscMaybe cId arch of
  Just as -> Map.fromAscList $ zip (Set.toList $ entities arch) (V.toList as)
  Nothing -> Map.empty
{-# INLINE lookupComponents #-}

-- | Lookup all components by their `ComponentID`, in ascending order by their `EntityID`.
lookupComponentsAsc :: forall m a. (Component m a) => ComponentID -> Archetype m -> Vector a
lookupComponentsAsc cId = fromMaybe V.empty . lookupComponentsAscMaybe @m @a cId
{-# INLINE lookupComponentsAsc #-}

-- | Lookup all components by their `ComponentID`, in ascending order by their `EntityID`.
lookupComponentsAscMaybe :: forall m a. (Component m a) => ComponentID -> Archetype m -> Maybe (Vector a)
lookupComponentsAscMaybe cId arch = S.toAscVector @a @(StorageT a) <$> lookupStorage @m @a cId arch
{-# INLINE lookupComponentsAscMaybe #-}

-- | Insert a component into the archetype.
-- This assumes the archetype contains one of each stored component per entity.
-- Returns the updated archetype and the onInsert hook to run.
insertComponent ::
  forall m a. (Component m a) => EntityID -> ComponentID -> a -> Archetype m -> (Archetype m, AccessT m ())
insertComponent e cId c arch =
  let !storage =
        S.fromAscVector @a @(StorageT a) . V.fromList . Map.elems . Map.insert e c $ lookupComponents cId arch
   in (arch {storages = IntMap.insert (unComponentId cId) (dynStorage @a storage) (storages arch)}, componentOnInsert e c)

-- | @True@ if this archetype contains an entity with the provided `ComponentID`.
member :: ComponentID -> Archetype m -> Bool
member cId = IntMap.member (unComponentId cId) . storages

-- | Zip a vector of components with a function and a component storage.
-- Returns the result vector, updated archetype, and the onChange hooks to run.
zipWith ::
  forall m a c. (Monad m, Component m c) => Vector a -> (a -> c -> c) -> ComponentID -> Archetype m -> (Vector c, Archetype m, AccessT m ())
zipWith as f cId arch =
  let go maybeDyn = case maybeDyn of
        Just dyn -> case fromDynamic $ storageDyn dyn of
          Just s -> do
            let !(cs', s') = S.zipWith @c @(StorageT c) f as s
            tell cs'
            return $ Just $ dyn {storageDyn = toDyn s'}
          Nothing -> return maybeDyn
        Nothing -> return Nothing
      (storages', cs) = runWriter $ IntMap.alterF go (unComponentId cId) $ storages arch
      eIds = V.fromList . Set.toList $ entities arch
      hooks = V.foldl (\acc (e, c) -> acc >> componentOnChange e c) (return ()) (V.zip eIds cs)
   in (cs, arch {storages = storages'}, hooks)
{-# INLINE zipWith #-}

-- | Zip a vector of components with a monadic function and a component storage.
-- Returns the result vector, updated archetype, and the onChange hooks to run.
zipWithM ::
  forall m a c. (Monad m, Component m c) => Vector a -> (a -> c -> m c) -> ComponentID -> Archetype m -> m (Vector c, Archetype m, AccessT m ())
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
  let cs = snd res
      eIds = V.fromList . Set.toList $ entities arch
      hooks = V.foldl (\acc (e, c) -> acc >> componentOnChange e c) (return ()) (V.zip eIds cs)
  return (cs, arch {storages = fst res}, hooks)

-- | Zip a vector of components with a function and a component storage.
-- Returns the updated archetype and the onChange hooks to run.
zipWith_ ::
  forall m a c. (Monad m, Component m c) => Vector a -> (a -> c -> c) -> ComponentID -> Archetype m -> (Archetype m, AccessT m ())
zipWith_ as f cId arch =
  let maybeStorage = case IntMap.lookup (unComponentId cId) $ storages arch of
        Just dyn -> case fromDynamic $ storageDyn dyn of
          Just s ->
            let !(cs, s') = S.zipWith @c @(StorageT c) f as s in Just (cs, dyn {storageDyn = toDyn s'})
          Nothing -> Nothing
        Nothing -> Nothing
   in case maybeStorage of
        Just (cs, s) ->
          let eIds = V.fromList . Set.toList $ entities arch
              hooks = V.foldl (\acc (e, c) -> acc >> componentOnChange e c) (return ()) (V.zip eIds cs)
           in (empty {storages = IntMap.singleton (unComponentId cId) s}, hooks)
        Nothing -> (empty {storages = IntMap.empty}, return ())
{-# INLINE zipWith_ #-}

-- | Insert a vector of components into the archetype, sorted in ascending order by their `EntityID`.
insertAscVector :: forall m a. (Component m a) => ComponentID -> Vector a -> Archetype m -> Archetype m
insertAscVector cId as arch =
  let !storage = dynStorage @a $ S.fromAscVector @a @(StorageT a) as
   in arch {storages = IntMap.insert (unComponentId cId) storage $ storages arch}
{-# INLINE insertAscVector #-}

-- | Remove an entity from an archetype, returning its components.
remove :: EntityID -> Archetype m -> (IntMap Dynamic, Archetype m)
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
removeStorages :: EntityID -> Archetype m -> (IntMap DynamicStorage, Archetype m)
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
insertComponents :: EntityID -> IntMap Dynamic -> Archetype m -> Archetype m
insertComponents e cs arch =
  let f archAcc (itemCId, dyn) =
        let storages' = IntMap.adjust go itemCId (storages archAcc)
            es = Set.toList $ entities archAcc
            go s =
              let ecs = V.fromList . Map.elems . Map.insert e dyn . Map.fromAscList . zip es . V.toList $ toAscVectorDyn s
               in fromAscVectorDyn ecs s
         in archAcc {storages = storages', entities = Set.insert e $ entities archAcc}
   in foldl' f arch (IntMap.toList cs)
