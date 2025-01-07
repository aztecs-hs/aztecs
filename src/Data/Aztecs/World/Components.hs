{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.World.Components
  ( ComponentID (..),
    Components (..),
    empty,
    insert,
    insert',
    lookup,
    lookup',
    getComponentId,
    insertComponentId,
  )
where

import Data.Aztecs.Component
import Data.Aztecs.Core (Entity)
import qualified Data.Aztecs.Storage as S
import Data.Data (TypeRep)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Typeable (Proxy (..), typeOf)
import Prelude hiding (lookup)

newtype ComponentID = ComponentID {unComponentId :: Int}
  deriving (Eq, Ord, Show)

data Components = Components (Map ComponentID Dynamic) (Map TypeRep ComponentID) ComponentID
  deriving (Show)

empty :: Components
empty = Components Map.empty Map.empty (ComponentID 0)

insert :: forall c. (Component c) => Entity -> c -> Components -> Components
insert e c cs =
  let (cId, cs') = insertComponentId @c cs
   in insert' e cId c cs'

insert' :: (Component c) => Entity -> ComponentID -> c -> Components -> Components
insert' e cId c (Components cs ids i) = do
  let f s = Just . toDyn $ S.insert (fromMaybe storage (s >>= fromDynamic)) e c
   in Components (Map.alter f cId cs) ids i

lookup :: forall c. (Component c) => Entity -> Components -> Maybe c
lookup e cs = case getComponentId @c cs of
  Just cId -> lookup' cId e cs
  Nothing -> Nothing

lookup' :: (Component c) => ComponentID -> Entity -> Components -> (Maybe c)
lookup' cId e (Components cs _ _) = do
  s <- Map.lookup cId cs
  s' <- fromDynamic s
  S.lookup s' e

getComponentId :: forall c. (Component c) => Components -> Maybe ComponentID
getComponentId (Components _ ids _) = Map.lookup (typeOf (Proxy @c)) ids

insertComponentId :: forall c. (Component c) => Components -> (ComponentID, Components)
insertComponentId (Components cs ids i) =
  case getComponentId @c (Components cs ids i) of
    Just _ -> (i, Components cs ids i)
    Nothing -> (i, Components cs (Map.insert (typeOf (Proxy @c)) i ids) (ComponentID (unComponentId i + 1)))
