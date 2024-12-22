{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.World
  ( Entity,
    EntityComponent (..),
    Component (..),
    World,
    union,
    spawn,
    insert,
    adjust,
    get,
    getRow,
    newWorld,
    setRow,
    remove,
  )
where

import Data.Aztecs.Core
import Data.Aztecs.Storage (Storage, table)
import qualified Data.Aztecs.Storage as S
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map (Map, alter, empty, lookup)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Typeable
import Prelude hiding (read)

class (Typeable a) => Component a where
  storage :: Storage a
  storage = table

data World = World (Map TypeRep Dynamic) Entity deriving (Show)

newWorld :: World
newWorld = World empty (Entity 0)

union :: World -> World -> World
union (World a e) (World b _) = World (Map.union a b) e

spawn :: forall c. (Component c) => c -> World -> (Entity, World)
spawn c (World w (Entity e)) = (Entity e, insert (Entity e) c (World w (Entity $ e + 1)))

insert :: forall c. (Component c) => Entity -> c -> World -> World
insert e c (World w e') =
  World
    ( alter
        (\maybeRow -> Just . toDyn $ S.spawn (fromMaybe storage (maybeRow >>= fromDynamic)) e c)
        (typeOf (Proxy :: Proxy c))
        w
    )
    e'

adjust :: (Component c) => c -> (c -> c) -> Entity -> World -> World
adjust a f w = insert w (f a)

getRow :: (Component c) => Proxy c -> World -> Maybe (Storage c)
getRow p (World w _) = Data.Map.lookup (typeOf p) w >>= fromDynamic

get :: forall c. (Component c) => Entity -> World -> Maybe c
get e (World w _) = Data.Map.lookup (typeOf @(Proxy c) Proxy) w >>= fromDynamic >>= flip S.get e

setRow :: forall c. (Component c) => Storage c -> World -> World
setRow cs (World w e') = World (Map.insert (typeOf @(Proxy c) Proxy) (toDyn cs) w) e'

remove :: forall c. (Component c) => Entity -> World -> World
remove e (World w e') = World (alter (\row -> row >>= f) (typeOf @(Proxy c) Proxy) w) e'
  where
    f row = fmap (\row' -> toDyn $ S.remove @c row' e) (fromDynamic row)
