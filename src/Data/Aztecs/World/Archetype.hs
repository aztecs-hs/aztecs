{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs.World.Archetype
  ( Archetype (..),
    empty,
    all,
    allMaybe,
    Lookup (..),
    lookupComponent,
    lookupStorage,
    member,
    remove,
    removeStorages,
    insertComponent,
    insertAscList,
    withAscList,
    Insert (..),
    AnyStorage (..),
    anyStorage,
  )
where

import Data.Aztecs.Component (Component (..), ComponentID)
import Data.Aztecs.Entity (Entity (..), EntityID (..))
import qualified Data.Aztecs.Storage as S
import Data.Aztecs.World.Components (Components)
import qualified Data.Aztecs.World.Components as CS
import Data.Bifunctor (Bifunctor (..))
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (all, lookup)

data AnyStorage = AnyStorage
  { storageDyn :: Dynamic,
    insertDyn :: Int -> Dynamic -> Dynamic -> Dynamic,
    removeDyn :: Int -> Dynamic -> (Maybe Dynamic, Dynamic),
    removeAny :: Int -> Dynamic -> (Maybe AnyStorage, Dynamic),
    entitiesDyn :: Dynamic -> [Int]
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
        Nothing -> (Nothing, dyn),
      entitiesDyn = \dyn -> case fromDynamic @(s a) dyn of
        Just s' -> map fst $ S.all s'
        Nothing -> []
    }

newtype Archetype = Archetype {storages :: Map ComponentID AnyStorage}
  deriving (Show)

empty :: Archetype
empty = Archetype {storages = Map.empty}

lookupStorage :: (Component a) => ComponentID -> Archetype -> Maybe (StorageT a a)
lookupStorage cId w = do
  dynS <- Map.lookup cId (storages w)
  fromDynamic (storageDyn dynS)

insertComponent :: forall a. (Component a) => EntityID -> ComponentID -> a -> Archetype -> Archetype
insertComponent e cId c arch =
  let storage = case lookupStorage cId arch of
        Just s -> S.insert (unEntityId e) c s
        Nothing -> S.singleton @(StorageT a) @a (unEntityId e) c
   in arch {storages = Map.insert cId (anyStorage storage) (storages arch)}

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
    (_, s) : _ -> map (\i -> (EntityID i, Nothing)) $ entitiesDyn s (storageDyn s)

class Lookup a where
  lookup :: EntityID -> Components -> Archetype -> Maybe a

instance Lookup (Entity '[]) where
  lookup _ _ _ = Just ENil

instance (Component a, Lookup (Entity as)) => Lookup (Entity (a ': as)) where
  lookup eId cs arch = do
    a <- lookupComponent eId (fromMaybe (error "TODO") (CS.lookup @a cs)) arch
    as <- lookup eId cs arch
    return $ ECons a as

lookupComponent :: forall a. (Component a) => EntityID -> ComponentID -> Archetype -> Maybe a
lookupComponent e cId w = lookupStorage cId w >>= S.lookup (unEntityId e)

insertAscList :: forall a. (Component a) => ComponentID -> [(EntityID, a)] -> Archetype -> Archetype
insertAscList cId as arch =
  arch
    { storages =
        Map.insert
          cId
          (anyStorage $ S.fromAscList @(StorageT a) (map (first unEntityId) as))
          (storages arch)
    }

withAscList :: forall a. (Component a) => ComponentID -> [a] -> Archetype -> Archetype
withAscList cId as arch =
  arch
    { storages =
        Map.adjust
          ( \s ->
              (anyStorage $ S.fromAscList @(StorageT a) (zip (entitiesDyn s (storageDyn s)) as))
          )
          cId
          (storages arch)
    }

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

class Insert a where
  insert :: EntityID -> a -> Archetype -> Components -> (Set ComponentID, Archetype, Components)

instance Insert (Entity '[]) where
  insert _ ENil arch cs = (Set.empty, arch, cs)

instance (Component a, Insert (Entity as)) => Insert (Entity (a ': as)) where
  insert eId (ECons e es) arch cs =
    let (cId, cs') = CS.insert @a cs
        arch' = insertComponent eId cId e arch
        (cIds, arch'', cs'') = insert eId es arch' cs'
     in (Set.insert cId cIds, arch'', cs'')
