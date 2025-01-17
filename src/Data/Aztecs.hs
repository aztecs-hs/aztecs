{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs where

import Control.Monad.Writer (MonadWriter (..), runWriter)
import Data.Aztecs.Storage (Storage)
import qualified Data.Aztecs.Storage as S
import Data.Data (Proxy (..), TypeRep, Typeable)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.IntMap (IntMap)
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (typeOf)
import Prelude hiding (all, lookup)

newtype EntityID = EntityID {unEntityId :: Int}
  deriving (Eq, Ord, Show)

class (Typeable a, Storage (StorageT a) a) => Component a where
  type StorageT a :: Type -> Type
  type StorageT a = IntMap

data World = World
  { storages :: Map TypeRep Dynamic,
    nextEntityId :: EntityID
  }
  deriving (Show)

empty :: World
empty =
  World
    { storages = Map.empty,
      nextEntityId = EntityID 0
    }

spawn :: forall a. (Component a) => a -> World -> (EntityID, World)
spawn c w =
  let e = nextEntityId w
   in (e, insert e c w {nextEntityId = EntityID (unEntityId e + 1)})

lookupStorage :: forall a. (Component a) => World -> Maybe (StorageT a a)
lookupStorage w = do
  dynS <- Map.lookup (typeOf (Proxy @a)) (storages w)
  fromDynamic dynS

insert :: forall a. (Component a) => EntityID -> a -> World -> World
insert e c w =
  let storage = case lookupStorage @a w of
        Just s -> S.insert (unEntityId e) c s
        Nothing -> S.singleton @(StorageT a) @a (unEntityId e) c
   in w {storages = Map.insert (typeOf (Proxy @a)) (toDyn storage) (storages w)}

all :: (Component a) => World -> [(EntityID, a)]
all w = fromMaybe [] $ do
  s <- lookupStorage w
  return . map (\(i, c) -> (EntityID i, c)) $ S.all s

allFilter :: (Component a, Typeable (StorageT a)) => Set EntityID -> World -> [a]
allFilter es w =
  let x = all w
   in Map.elems $ Map.restrictKeys (Map.fromList x) es

mapComponents :: forall a. (Component a) => (a -> a) -> World -> World
mapComponents f w =
  let go dynS = case dynS >>= fromDynamic @((StorageT a) a) of
        Just s -> return . Just . toDyn $ S.mapComponents f s
        Nothing -> return $ Nothing
   in fromMaybe w $ do
        storages' <- Map.alterF go (typeOf (Proxy @a)) (storages w)
        return w {storages = storages'}

mapComponentsFilter :: forall a. (Component a) => [EntityID] -> (a -> a) -> World -> ([a], World)
mapComponentsFilter es f w =
  let go dynS = case dynS >>= fromDynamic @((StorageT a) a) of
        Just s -> do
          let (csAcc, sAcc) = S.mapComponentsFilter (fmap unEntityId es) f s
          tell csAcc
          return . Just $ toDyn sAcc
        Nothing -> return $ Nothing
      (s'', cs) = runWriter $ Map.alterF go (typeOf (Proxy @a)) (storages w)
   in (map snd cs, w {storages = s''})

lookup :: forall a. (Component a) => EntityID -> World -> Maybe a
lookup e w = lookupStorage w >>= S.lookup (unEntityId e)
