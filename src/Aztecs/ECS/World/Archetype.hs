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
    lookupStorage,
    member,
    remove,
    removeStorages,
    insertComponent,
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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

lookupStorage :: (Component a) => ComponentID -> Archetype -> Maybe (StorageT a)
lookupStorage cId w = do
  dynS <- Map.lookup cId (storages w)
  fromDynamic (storageDyn dynS)

lookupComponents :: forall a. (Component a) => ComponentID -> Archetype -> Map EntityID a
lookupComponents cId arch = case lookupStorage @a cId arch of
  Just s -> Map.fromAscList . zip (Set.toList $ entities arch) $ S.toAscList s
  Nothing -> Map.empty

insertComponent :: forall a. (Component a) => EntityID -> ComponentID -> a -> Archetype -> Archetype
insertComponent e cId c arch =
  let !storage =
        S.fromAscList @a @(StorageT a) . Map.elems . Map.insert e c $ lookupComponents cId arch
   in arch {storages = Map.insert cId (dynStorage @a storage) (storages arch)}

member :: ComponentID -> Archetype -> Bool
member cId arch = Map.member cId (storages arch)

lookupComponent :: forall a. (Component a) => EntityID -> ComponentID -> Archetype -> Maybe a
lookupComponent e cId w = lookupComponents cId w Map.!? e

-- | Insert a list of components into the archetype, sorted in ascending order by their `EntityID`.
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
