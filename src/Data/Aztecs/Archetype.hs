{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.Archetype where

import Data.Aztecs (Component (..), ComponentID, EntityID (..))
import qualified Data.Aztecs.Storage as S
import Data.Bifunctor (Bifunctor (..))
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Prelude hiding (all, lookup)

data AnyStorage = AnyStorage
  { storageDyn :: Dynamic,
    insertDyn :: Int -> Dynamic -> Dynamic -> Dynamic,
    removeDyn :: Int -> Dynamic -> (Maybe Dynamic, Dynamic),
    removeAny :: Int -> Dynamic -> (Maybe AnyStorage, Dynamic)
  }

instance Show AnyStorage where
  show s = "AnyStorage " ++ show (storageDyn s)

anyStorage :: forall s a. (S.Storage s a) => s a -> AnyStorage
anyStorage s =
  AnyStorage
    { storageDyn = toDyn s,
      insertDyn = \i cDyn sDyn ->
        fromMaybe sDyn $ do
          s' <- fromDynamic @(s a) sDyn
          c <- fromDynamic cDyn
          return . toDyn $ S.insert i c s',
      removeDyn = \i dyn -> case fromDynamic @(s a) dyn of
        Just s' -> let (a, b) = S.remove i s' in (fmap toDyn a, toDyn b)
        Nothing -> (Nothing, dyn),
      removeAny = \i dyn -> case fromDynamic @(s a) dyn of
        Just s' -> let (a, b) = S.remove i s' in (fmap (anyStorage . S.singleton @s i) a, toDyn b)
        Nothing -> (Nothing, dyn)
    }

newtype Archetype = Archetype {storages :: Map ComponentID AnyStorage}
  deriving (Show)

empty :: Archetype
empty = Archetype {storages = Map.empty}

lookupStorage :: (Component a) => ComponentID -> Archetype -> Maybe (StorageT a a)
lookupStorage cId w = do
  dynS <- Map.lookup cId (storages w)
  fromDynamic (storageDyn dynS)

insert :: forall a. (Component a) => EntityID -> ComponentID -> a -> Archetype -> Archetype
insert e cId c arch =
  let storage = case lookupStorage cId arch of
        Just s -> S.insert (unEntityId e) c s
        Nothing -> S.singleton @(StorageT a) @a (unEntityId e) c
   in arch {storages = Map.insert cId (anyStorage storage) (storages arch)}

all :: (Component a) => ComponentID -> Archetype -> [(EntityID, a)]
all cId arch = fromMaybe [] $ do
  s <- lookupStorage cId arch
  return . map (first EntityID) $ S.all s

lookup :: forall a. (Component a) => EntityID -> ComponentID -> Archetype -> Maybe a
lookup e cId w = lookupStorage cId w >>= S.lookup (unEntityId e)

insertAscList :: forall a. (Component a) => ComponentID -> [(EntityID, a)] -> Archetype -> Archetype
insertAscList cId as arch = arch {storages = Map.insert cId (anyStorage $ S.fromAscList @(StorageT a) (map (first unEntityId) as)) (storages arch)}

remove :: EntityID -> Archetype -> (Map ComponentID Dynamic, Archetype)
remove e arch =
  foldr
    ( \(cId, s) (dynAcc, archAcc) ->
        let (dynA, dynS) = removeDyn s (unEntityId e) (storageDyn s)
            dynAcc' = case dynA of
              Just d -> Map.insert cId d dynAcc
              Nothing -> dynAcc
         in ( dynAcc',
              archAcc {storages = Map.insert cId (s {storageDyn = dynS}) (storages archAcc)}
            )
    )
    (Map.empty, arch)
    (Map.toList $ storages arch)

removeStorages :: EntityID -> Archetype -> (Map ComponentID AnyStorage, Archetype)
removeStorages e arch =
  foldr
    ( \(cId, s) (dynAcc, archAcc) ->
        let (dynA, dynS) = removeAny s (unEntityId e) (storageDyn s)
            dynAcc' = case dynA of
              Just d -> Map.insert cId d dynAcc
              Nothing -> dynAcc
         in ( dynAcc',
              archAcc {storages = Map.insert cId (s {storageDyn = dynS}) (storages archAcc)}
            )
    )
    (Map.empty, arch)
    (Map.toList $ storages arch)
