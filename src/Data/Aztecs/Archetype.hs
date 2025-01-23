{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.Archetype where

import Control.Monad.Writer (MonadWriter (..), runWriter)
import Data.Aztecs.Entity (EntityID (..))
import Data.Aztecs.Storage (Storage)
import qualified Data.Aztecs.Storage as S
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy (..), TypeRep, Typeable)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.IntMap (IntMap)
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Typeable (typeOf)
import Prelude hiding (all, lookup)

class (Typeable a, Storage (StorageT a) a) => Component a where
  type StorageT a :: Type -> Type
  type StorageT a = IntMap

newtype Archetype = Archetype
  { storages :: Map TypeRep Dynamic
  }
  deriving (Show)

empty :: Archetype
empty = Archetype {storages = Map.empty}

lookupStorage :: forall a. (Component a) => Archetype -> Maybe (StorageT a a)
lookupStorage w = do
  dynS <- Map.lookup (typeOf (Proxy @a)) (storages w)
  fromDynamic dynS

insert :: forall a. (Component a) => EntityID -> a -> Archetype -> Archetype
insert e c w =
  let storage = case lookupStorage @a w of
        Just s -> S.insert (unEntityId e) c s
        Nothing -> S.singleton @(StorageT a) @a (unEntityId e) c
   in w {storages = Map.insert (typeOf (Proxy @a)) (toDyn storage) (storages w)}

all :: (Component a) => Archetype -> [(EntityID, a)]
all w = fromMaybe [] $ do
  s <- lookupStorage w
  return . map (first EntityID) $ S.all s

allFilter :: (Component a, Typeable (StorageT a)) => Set EntityID -> Archetype -> [a]
allFilter es w =
  let x = all w
   in Map.elems $ Map.restrictKeys (Map.fromList x) es

mapComponents :: forall a. (Component a) => (a -> a) -> Archetype -> Archetype
mapComponents f w =
  let go dynS = case dynS >>= fromDynamic @((StorageT a) a) of
        Just s -> return . Just . toDyn $ S.mapComponents f s
        Nothing -> return Nothing
   in fromMaybe w $ do
        storages' <- Map.alterF go (typeOf (Proxy @a)) (storages w)
        return w {storages = storages'}

mapComponentsFilter :: forall a. (Component a) => [EntityID] -> (a -> a) -> Archetype -> ([a], Archetype)
mapComponentsFilter es f w =
  let go dynS = case dynS >>= fromDynamic @((StorageT a) a) of
        Just s -> do
          let (csAcc, sAcc) = S.mapComponentsFilter (fmap unEntityId es) f s
          tell csAcc
          return . Just $ toDyn sAcc
        Nothing -> return Nothing
      (s'', cs) = runWriter $ Map.alterF go (typeOf (Proxy @a)) (storages w)
   in (map snd cs, w {storages = s''})

lookup :: forall a. (Component a) => EntityID -> Archetype -> Maybe a
lookup e w = lookupStorage w >>= S.lookup (unEntityId e)
