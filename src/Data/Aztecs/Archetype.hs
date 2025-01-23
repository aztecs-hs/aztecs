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

newtype Archetype = Archetype {storages :: Map ComponentID Dynamic}
  deriving (Show)

empty :: Archetype
empty = Archetype {storages = Map.empty}

lookupStorage :: Component a => ComponentID -> Archetype -> Maybe (StorageT a a)
lookupStorage cId w = do
  dynS <- Map.lookup cId (storages w)
  fromDynamic dynS

insert :: forall a. (Component a) => EntityID -> ComponentID -> a -> Archetype -> Archetype
insert e cId c arch =
  let storage = case lookupStorage cId arch of
        Just s -> S.insert (unEntityId e) c s
        Nothing -> S.singleton @(StorageT a) @a (unEntityId e) c
   in arch {storages = Map.insert cId (toDyn storage) (storages arch)}

all :: (Component a) => ComponentID -> Archetype -> [(EntityID, a)]
all cId arch = fromMaybe [] $ do
  s <- lookupStorage cId arch
  return . map (first EntityID) $ S.all s

lookup :: forall a. (Component a) => EntityID -> ComponentID -> Archetype -> Maybe a
lookup e cId w = lookupStorage cId w >>= S.lookup (unEntityId e)

insertAscList :: forall a. (Component a) => ComponentID -> [(EntityID, a)] -> Archetype -> Archetype
insertAscList cId as arch = arch {storages = Map.insert cId (toDyn $ S.fromAscList @(StorageT a) (map (first unEntityId) as)) (storages arch)}
