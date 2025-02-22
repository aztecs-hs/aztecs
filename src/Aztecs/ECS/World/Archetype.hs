{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
{-# LANGUAGE TypeOperators #-}

module Aztecs.ECS.World.Archetype
  ( Archetype (..),
    empty,
    all,
    allMaybe,
    entities,
    lookupComponent,
    lookupStorage,
    member,
    remove,
    removeStorages,
    insertComponent,
    withAscList,
  )
where

import Aztecs.ECS.Component (Component (..), ComponentID)
import Aztecs.ECS.Entity (EntityID (..))
import qualified Aztecs.ECS.World.Storage as S
import Aztecs.ECS.World.Storage.Dynamic
import Control.DeepSeq
import Data.Bifunctor (Bifunctor (..))
import Data.Dynamic (Dynamic, fromDynamic)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Prelude hiding (all, lookup)

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif

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

all :: (Component a) => ComponentID -> Archetype -> [(EntityID, a)]
all cId arch = fromMaybe [] $ do
  s <- lookupStorage cId arch
  return . map (first EntityID) $ S.all s

member :: ComponentID -> Archetype -> Bool
member cId arch = Map.member cId (storages arch)

allMaybe :: (Component a) => ComponentID -> Archetype -> [(EntityID, Maybe a)]
allMaybe cId arch = case lookupStorage cId arch of
  Just s -> map (\(i, a) -> (EntityID i, Just a)) $ S.all s
  Nothing -> case Map.toList $ storages arch of
    [] -> []
    (_, s) : _ -> map (\i -> (EntityID i, Nothing)) $ entitiesDyn s

entities :: Archetype -> [EntityID]
entities arch = case Map.toList $ storages arch of
  [] -> []
  (_, s) : _ -> map (\i -> (EntityID i)) $ entitiesDyn s

lookupComponent :: forall a. (Component a) => EntityID -> ComponentID -> Archetype -> Maybe a
lookupComponent e cId w = lookupStorage cId w >>= S.lookup (unEntityId e)

withAscList :: forall a. (Component a) => ComponentID -> [a] -> Archetype -> Archetype
withAscList cId as arch =
  let !storages' =
        Map.adjust
          ( \s ->
              (dynStorage $ S.fromAscList @(StorageT a) (zip (entitiesDyn s) as))
          )
          cId
          (storages arch)
   in arch {storages = storages'}

remove :: EntityID -> Archetype -> (Map ComponentID Dynamic, Archetype)
remove e arch =
  foldl'
    ( \(dynAcc, archAcc) (cId, s) ->
        let !(dynA, dynS) = removeDyn  (unEntityId e) s
            !dynAcc' = case dynA of
              Just d -> Map.insert cId d dynAcc
              Nothing -> dynAcc
         in ( dynAcc',
              archAcc {storages = Map.insert cId dynS $ storages archAcc}
            )
    )
    (Map.empty, arch)
    (Map.toList $ storages arch)

removeStorages :: EntityID -> Archetype -> (Map ComponentID DynamicStorage, Archetype)
removeStorages e arch =
  foldl'
    ( \(dynAcc, archAcc) (cId, s) ->
        let (dynA, dynS) = removeAny  (unEntityId e) s
            dynAcc' = case dynA of
              Just d -> Map.insert cId d dynAcc
              Nothing -> dynAcc
         in ( dynAcc',
              archAcc {storages = Map.insert cId dynS $ storages archAcc}
            )
    )
    (Map.empty, arch)
    (Map.toList $ storages arch)
