{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.World
  ( Entity,
    EntityComponent (..),
    Component (..),
    World (..),
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
import Data.Aztecs.Storage (Storage)
import Data.Aztecs.World.Archetypes (Archetypes, newArchetypes)
import qualified Data.Aztecs.World.Archetypes as A
import Data.Aztecs.World.Components (Component, Components, newComponents)
import qualified Data.Aztecs.World.Components as C
import Data.Typeable
import Prelude hiding (read)

data World = World Components Archetypes deriving (Show)

newWorld :: World
newWorld = World newComponents newArchetypes

union :: World -> World -> World
union (World cs as) (World cs' _) = World (C.union cs cs') as

spawn :: forall c. (Component c) => c -> World -> (Entity, World)
spawn c (World cs as) = let (e, cs') = C.spawn c cs in (e, World cs' as)

insert :: forall c. (Component c) => Entity -> c -> World -> World
insert e c (World cs as) =
  let cs' = C.insert e c cs
   in World cs' (A.insert @c e cs' as)

adjust :: (Component c) => c -> (c -> c) -> Entity -> World -> World
adjust a f w = insert w (f a)

getRow :: (Component c) => Proxy c -> World -> Maybe (Storage c)
getRow p (World cs _) = C.getRow p cs

get :: forall c. (Component c) => Entity -> World -> Maybe c
get e (World cs _) = C.get e cs

setRow :: forall c. (Component c) => Storage c -> World -> World
setRow row (World cs as) = World (C.setRow row cs) as

remove :: forall c. (Component c) => Entity -> World -> World
remove e (World cs as) = World (C.remove @c e cs) as
