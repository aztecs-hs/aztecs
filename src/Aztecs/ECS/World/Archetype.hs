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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics

data Archetype = Archetype
  { storages :: !(Map ComponentID DynamicStorage),
    entities :: !(Set EntityID)
  }
  deriving (Show, Generic, NFData)

empty :: Archetype
empty = Archetype {storages = Map.empty, entities = Set.empty}

{-# INLINE lookupStorage #-}
lookupStorage :: (Component a) => ComponentID -> Archetype -> Maybe (StorageT a)
lookupStorage cId w = do
  dynS <- Map.lookup cId (storages w)
  fromDynamic (storageDyn dynS)

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
   in arch {storages = Map.insert cId (dynStorage @a storage) (storages arch)}

member :: ComponentID -> Archetype -> Bool
member cId arch = Map.member cId (storages arch)

-- | Insert a list of components into the archetype, sorted in ascending order by their `EntityID`.
{-# INLINE insertAscList #-}
insertAscList :: forall a. (Component a) => ComponentID -> [a] -> Archetype -> Archetype
insertAscList cId as arch =
  let !storage = dynStorage @a $ S.fromAscList @a @(StorageT a) as
   in arch {storages = Map.insert cId storage (storages arch)}

remove :: EntityID -> Archetype -> (Map ComponentID Dynamic, Archetype)
remove e arch =
  foldl'
    ( \(dynAcc, archAcc) (cId, dynS) ->
        let cs = Map.fromAscList . zip (Set.toList $ entities arch) $ toAscListDyn dynS
            !(dynA, cs') = Map.updateLookupWithKey (\_ _ -> Nothing) e cs
            dynS' = S.fromAscListDyn (Map.elems cs') dynS
            !dynAcc' = case dynA of
              Just d -> Map.insert cId d dynAcc
              Nothing -> dynAcc
         in (dynAcc', archAcc {storages = Map.insert cId dynS' $ storages archAcc})
    )
    (Map.empty, arch)
    (Map.toList $ storages arch)

removeStorages :: EntityID -> Archetype -> (Map ComponentID DynamicStorage, Archetype)
removeStorages e arch =
  foldl'
    ( \(dynAcc, archAcc) (cId, dynS) ->
        let cs = Map.fromAscList . zip (Set.toList $ entities arch) $ toAscListDyn dynS
            !(dynA, cs') = Map.updateLookupWithKey (\_ _ -> Nothing) e cs
            dynS' = S.fromAscListDyn (Map.elems cs') dynS
            !dynAcc' = case dynA of
              Just d -> Map.insert cId (S.singletonDyn d dynS') dynAcc
              Nothing -> dynAcc
         in (dynAcc', archAcc {storages = Map.insert cId dynS' $ storages archAcc})
    )
    (Map.empty, arch)
    (Map.toList $ storages arch)

insertComponents :: EntityID -> Map ComponentID Dynamic -> Archetype -> Archetype
insertComponents e cs arch =
  let f archAcc (itemCId, dyn) =
        let storages' = Map.adjust go itemCId (storages archAcc)
            go s = fromAscListDyn (Map.elems . Map.insert e dyn . Map.fromAscList . zip (Set.toList $ entities archAcc) $ toAscListDyn s) s
         in archAcc {storages = storages'}
   in foldl' f arch (Map.toList cs)
