{-# LANGUAGE AllowAmbiguousTypes #-}
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

module Aztecs.ECS.World.Archetype
  ( Archetype (..),
    empty,
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
    zipWith,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import qualified Aztecs.ECS.World.Storage as S
import Aztecs.ECS.World.Storage.Dynamic
import qualified Aztecs.ECS.World.Storage.Dynamic as S
import Control.DeepSeq
import Control.Monad.Writer (MonadWriter (..), runWriter)
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
import Prelude hiding (map, zipWith)

data Archetype = Archetype
  { storages :: !(IntMap DynamicStorage),
    entities :: !(Set EntityID)
  }
  deriving (Show, Generic, NFData)

empty :: Archetype
empty = Archetype {storages = IntMap.empty, entities = Set.empty}

{-# INLINE lookupStorage #-}
lookupStorage :: (Component a) => ComponentID -> Archetype -> Maybe (StorageT a)
lookupStorage cId w = do
  !dynS <- IntMap.lookup (unComponentId cId) $ storages w
  fromDynamic $ storageDyn dynS

{-# INLINE lookupComponent #-}
lookupComponent :: (Component a) => EntityID -> ComponentID -> Archetype -> Maybe a
lookupComponent e cId w = lookupComponents cId w Map.!? e

{-# INLINE lookupComponents #-}
lookupComponents :: (Component a) => ComponentID -> Archetype -> Map EntityID a
lookupComponents cId arch = case lookupComponentsAscMaybe cId arch of
  Just as -> Map.fromAscList $ zip (Set.toList $ entities arch) as
  Nothing -> Map.empty

{-# INLINE lookupComponentsAsc #-}
lookupComponentsAsc :: (Component a) => ComponentID -> Archetype -> [a]
lookupComponentsAsc cId = fromMaybe [] . lookupComponentsAscMaybe cId

{-# INLINE lookupComponentsAscMaybe #-}
lookupComponentsAscMaybe :: forall a. (Component a) => ComponentID -> Archetype -> Maybe [a]
lookupComponentsAscMaybe cId arch = S.toAscList <$> lookupStorage @a cId arch

insertComponent :: forall a. (Component a) => EntityID -> ComponentID -> a -> Archetype -> Archetype
insertComponent e cId c arch =
  let !storage =
        S.fromAscList @a @(StorageT a) . Map.elems . Map.insert e c $ lookupComponents cId arch
   in arch {storages = IntMap.insert (unComponentId cId) (dynStorage @a storage) (storages arch)}

member :: ComponentID -> Archetype -> Bool
member cId = IntMap.member (unComponentId cId) . storages

{-# INLINE zipWith #-}
zipWith :: forall a c. (Component c) => [a] -> (a -> c -> c) -> ComponentID -> Archetype -> ([c], Archetype)
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

-- | Insert a list of components into the archetype, sorted in ascending order by their `EntityID`.
{-# INLINE insertAscList #-}
insertAscList :: forall a. (Component a) => ComponentID -> [a] -> Archetype -> Archetype
insertAscList cId as arch =
  let !storage = dynStorage @a $ S.fromAscList @a @(StorageT a) as
   in arch {storages = IntMap.insert (unComponentId cId) storage $ storages arch}

remove :: EntityID -> Archetype -> (IntMap Dynamic, Archetype)
remove e arch =
  foldl'
    ( \(dynAcc, archAcc) (cId, dynS) ->
        let cs = Map.fromAscList . zip (Set.toList $ entities arch) $ toAscListDyn dynS
            !(dynA, cs') = Map.updateLookupWithKey (\_ _ -> Nothing) e cs
            dynS' = S.fromAscListDyn (Map.elems cs') dynS
            !dynAcc' = case dynA of
              Just d -> IntMap.insert cId d dynAcc
              Nothing -> dynAcc
         in (dynAcc', archAcc {storages = IntMap.insert cId dynS' $ storages archAcc})
    )
    (IntMap.empty, arch)
    (IntMap.toList $ storages arch)

removeStorages :: EntityID -> Archetype -> (IntMap DynamicStorage, Archetype)
removeStorages e arch =
  foldl'
    ( \(dynAcc, archAcc) (cId, dynS) ->
        let cs = Map.fromAscList . zip (Set.toList $ entities arch) $ toAscListDyn dynS
            !(dynA, cs') = Map.updateLookupWithKey (\_ _ -> Nothing) e cs
            dynS' = S.fromAscListDyn (Map.elems cs') dynS
            !dynAcc' = case dynA of
              Just d -> IntMap.insert cId (S.singletonDyn d dynS') dynAcc
              Nothing -> dynAcc
         in (dynAcc', archAcc {storages = IntMap.insert cId dynS' $ storages archAcc})
    )
    (IntMap.empty, arch)
    (IntMap.toList $ storages arch)

insertComponents :: EntityID -> IntMap Dynamic -> Archetype -> Archetype
insertComponents e cs arch =
  let f archAcc (itemCId, dyn) =
        let storages' = IntMap.adjust go itemCId (storages archAcc)
            go s = fromAscListDyn (Map.elems . Map.insert e dyn . Map.fromAscList . zip (Set.toList $ entities archAcc) $ toAscListDyn s) s
         in archAcc {storages = storages'}
   in foldl' f arch (IntMap.toList cs)
