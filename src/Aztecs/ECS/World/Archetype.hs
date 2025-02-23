{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.World.Archetype
  ( Archetype (..),
    empty,
    entities,
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
import Control.DeepSeq
import Data.Bifunctor
import Data.Dynamic
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics

newtype Archetype = Archetype {storages :: Map ComponentID DynamicStorage}
  deriving (Show, Generic, NFData)

empty :: Archetype
empty = Archetype {storages = Map.empty}

lookupStorage :: (Component a) => ComponentID -> Archetype -> Maybe (StorageT a a)
lookupStorage cId w = do
  dynS <- Map.lookup cId (storages w)
  fromDynamic (storageDyn dynS)

insertComponent :: forall a. (Component a) => EntityID -> ComponentID -> a -> Archetype -> Archetype
insertComponent e cId c arch =
  let !storage = case lookupStorage cId arch of
        Just s -> S.insert (unEntityId e) c s
        Nothing -> S.singleton @(StorageT a) @a (unEntityId e) c
   in arch {storages = Map.insert cId (dynStorage storage) (storages arch)}

member :: ComponentID -> Archetype -> Bool
member cId arch = Map.member cId (storages arch)

entities :: Archetype -> [EntityID]
entities arch = case Map.toList $ storages arch of
  [] -> []
  (_, s) : _ -> map EntityID $ entitiesDyn s

lookupComponent :: forall a. (Component a) => EntityID -> ComponentID -> Archetype -> Maybe a
lookupComponent e cId w = lookupStorage cId w >>= S.lookup (unEntityId e)

-- | Insert a list of components into the archetype, sorted in ascending order by their `EntityID`.
insertAscList :: forall a. (Component a) => ComponentID -> [(EntityID, a)] -> Archetype -> Archetype
insertAscList cId as arch =
  let !storage = dynStorage $ S.fromAscList @(StorageT a) (map (first unEntityId) as)
   in arch {storages = Map.insert cId storage (storages arch)}

remove :: EntityID -> Archetype -> (Map ComponentID Dynamic, Archetype)
remove e arch =
  foldl'
    ( \(dynAcc, archAcc) (cId, s) ->
        let !(dynA, dynS) = removeDyn (unEntityId e) s
            !dynAcc' = case dynA of
              Just d -> Map.insert cId d dynAcc
              Nothing -> dynAcc
         in (dynAcc', archAcc {storages = Map.insert cId dynS $ storages archAcc})
    )
    (Map.empty, arch)
    (Map.toList $ storages arch)

removeStorages :: EntityID -> Archetype -> (Map ComponentID DynamicStorage, Archetype)
removeStorages e arch =
  foldl'
    ( \(dynAcc, archAcc) (cId, s) ->
        let (dynA, dynS) = removeAny (unEntityId e) s
            dynAcc' = case dynA of
              Just d -> Map.insert cId d dynAcc
              Nothing -> dynAcc
         in (dynAcc', archAcc {storages = Map.insert cId dynS $ storages archAcc})
    )
    (Map.empty, arch)
    (Map.toList $ storages arch)
