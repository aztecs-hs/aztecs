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
  )
where

import Data.Aztecs.Core
import Data.Aztecs.Storage (Storage, table)
import qualified Data.Aztecs.Storage as S
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map (Map, alter, empty, lookup)
import qualified Data.Map as Map
import Data.Typeable
import Prelude hiding (read)

class Component a where
  storage :: Storage a
  storage = table

data World = World (Map TypeRep Dynamic) Entity deriving (Show)

newWorld :: World
newWorld = World empty (Entity 0)

union :: World -> World -> World
union (World a e) (World b _) = World (Map.union a b) e

spawn :: (Component c, Typeable c) => c -> World -> (Entity, World)
spawn = f (Proxy)
  where
    f :: (Component c, Typeable c) => Proxy c -> c -> World -> (Entity, World)
    f p c (World w (Entity e)) =
      ( Entity e,
        World
          ( alter
              ( \maybeRow -> case maybeRow of
                  Just row -> fmap (\row' -> toDyn $ S.spawn row' (Entity e) c) (fromDynamic row)
                  Nothing -> Just $ toDyn $ S.spawn storage (Entity e) c
              )
              (typeOf p)
              w
          )
          (Entity (e + 1))
      )

insert :: (Component c, Typeable c) => Entity -> c -> World -> World
insert = f Proxy
  where
    f :: (Component c, Typeable c) => Proxy c -> Entity -> c -> World -> World
    f p e c (World w e') =
      World
        ( alter
            ( \maybeRow -> Just $ toDyn $ g (maybeRow >>= fromDynamic) e c
            )
            (typeOf p)
            w
        )
        e'
    g :: (Component c, Typeable c) => Maybe (Storage c) -> Entity -> c -> Storage c
    g maybeRow e c = case maybeRow of
      Just row -> S.spawn row e c
      Nothing -> S.spawn storage e c

adjust :: (Component c, Typeable c) => c -> (c -> c) -> Entity -> World -> World
adjust a f w = insert w (f a)

getRow :: (Typeable c) => Proxy c -> World -> Maybe (Storage c)
getRow p (World w _) = Data.Map.lookup (typeOf p) w >>= fromDynamic

get :: forall c. (Typeable c) => Entity -> World -> Maybe c
get e (World w _) = Data.Map.lookup (typeOf @(Proxy c) Proxy) w >>= fromDynamic >>= flip S.get e

setRow :: forall c. (Component c, Typeable c) => Storage c -> World -> World
setRow cs (World w e') = World (Map.insert (typeOf @(Proxy c) Proxy) (toDyn cs) w) e'
