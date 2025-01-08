{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.World.Components
  ( ComponentID (..),
    ComponentRef (..),
    componentRef,
    Components (..),
    empty,
    insert,
    insert',
    lookup,
    lookup',
    getComponentId,
    insertComponentId,
    insertComponent,
    lookupComponent,
    lookupStorage,
  )
where

import Data.Aztecs.Component
import Data.Aztecs.Core (Entity)
import Data.Aztecs.Storage (ComponentStorage (..))
import qualified Data.Aztecs.Storage as S
import Data.Data (TypeRep, Typeable)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Typeable (Proxy (..), typeOf)
import Prelude hiding (lookup)

newtype ComponentID = ComponentID {unComponentId :: Int}
  deriving (Eq, Ord, Show)

newtype ComponentRef c = ComponentRef (Components -> c)

data Components = Components (Map ComponentID Dynamic) (Map TypeRep ComponentID) ComponentID
  deriving (Show)

empty :: Components
empty = Components Map.empty Map.empty (ComponentID 0)

insert :: forall c. (Component c) => Entity -> c -> Components -> (ComponentRef c, Components)
insert e c cs =
  let (cId, cs') = insertComponentId @c cs
   in insert' e cId c cs'

insert' :: forall c. (Component c) => Entity -> ComponentID -> c -> Components -> (ComponentRef c, Components)
insert' e cId c (Components cs ids i) =
  let s' = fromMaybe (storage @c) (Map.lookup cId cs >>= fromDynamic)
      (f, y) = insertComponent e c s'
      g = \(Components cs' _ _) -> f $ fromMaybe (error "TODO") (Map.lookup cId cs')
   in (ComponentRef g, Components (Map.insert cId (toDyn y) cs) ids i)

lookup :: forall c. (Component c) => Entity -> Components -> Maybe c
lookup e cs = case getComponentId @c cs of
  Just cId -> lookup' cId e cs
  Nothing -> Nothing

lookup' :: (Component c) => ComponentID -> Entity -> Components -> (Maybe c)
lookup' cId e (Components cs _ _) = do
  s <- Map.lookup cId cs
  s' <- fromDynamic s
  lookupComponent e s'

lookupStorage :: forall a c. (Typeable a, Component c) => Components -> Maybe (ComponentStorage a c)
lookupStorage cs = case getComponentId @c cs of
  Just cId -> lookupStorage' cId cs
  Nothing -> Nothing

lookupStorage' :: (Typeable a, Component c) => ComponentID -> Components -> Maybe (ComponentStorage a c)
lookupStorage' cId (Components cs _ _) = do
  s <- Map.lookup cId cs
  fromDynamic s

getComponentId :: forall c. (Component c) => Components -> Maybe ComponentID
getComponentId (Components _ ids _) = Map.lookup (typeOf (Proxy @c)) ids

insertComponentId :: forall c. (Component c) => Components -> (ComponentID, Components)
insertComponentId (Components cs ids i) =
  case getComponentId @c (Components cs ids i) of
    Just i' -> (i', Components cs ids i)
    Nothing -> (i, Components cs (Map.insert (typeOf (Proxy @c)) i ids) (ComponentID (unComponentId i + 1)))

insertComponent :: (Typeable c) => Entity -> c -> ComponentStorage a c -> (Dynamic -> c, ComponentStorage a c)
insertComponent e c (ComponentStorage s) =
  let (f, s') = S.insert e c s
      g x = f $ fromMaybe (error "TODO") (fromDynamic x)
   in (g, ComponentStorage s')

lookupComponent :: Entity -> ComponentStorage c c -> Maybe c
lookupComponent e (ComponentStorage s) = S.lookup e s

componentRef :: (Dynamic -> c) -> ComponentID -> ComponentRef c
componentRef f cId = ComponentRef $ \(Components cs _ _) -> f $ fromMaybe (error "TODO") (Map.lookup cId cs)
