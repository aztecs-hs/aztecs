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
    get,
    newWorld,
    remove,
  )
where

import Data.Aztecs.Core
import Data.Aztecs.World.Archetypes (Archetypes, newArchetypes)
import qualified Data.Aztecs.World.Archetypes as A
import Data.Aztecs.World.Components (Component, Components, newComponents)
import qualified Data.Aztecs.World.Components as C
import Prelude hiding (read)

data World = World Components Archetypes deriving (Show)

newWorld :: World
newWorld = World newComponents newArchetypes

union :: World -> World -> World
union (World cs as) (World cs' _) = World (C.union cs cs') as

-- | Spawn an entity with a component.
--
-- == Examples
--
-- >>> :set -XTypeApplications
-- >>>
-- >>> -- Define a component.
-- >>> newtype Position = Position Int deriving (Show)
-- >>>
-- >>> instance Component Position
-- >>>
-- >>> -- Spawn an entity with a `Position` component.
-- >>> (e, w) <- spawn (Position 0) newWorld
-- >>>
-- >>> -- Get the `Position` component of the newly spawned entity.
-- >>> res <- get @Position e w
-- >>> res
-- Just (Position 0)
spawn :: forall c. (Component c) => c -> World -> IO (Entity, World)
spawn c (World cs as) = do
  (e, cs') <- C.spawn c cs
  return (e, World cs' as)

insert :: forall c. (Component c) => Entity -> c -> World -> IO World
insert e c (World cs as) = do
  (i, cs') <- C.insert e c cs
  return $ World cs' (A.insert @c i e cs' as)

get :: forall c. (Component c) => Entity -> World -> IO (Maybe c)
get e w = do
  c <- get' e w
  case c of
    Just (c', _) -> return $ Just c'
    Nothing -> return Nothing

get' :: forall c. (Component c) => Entity -> World -> IO (Maybe (c, c -> World -> IO World))
get' e (World cs _) = do
  res <- C.get e cs
  case res of
    Just (c, f) ->
      return $
        Just
          ( c,
            \c' (World cs' as) -> do
              cs'' <- f c' cs'
              return $ World cs'' as
          )
    Nothing -> return Nothing

remove :: forall c. (Component c) => Entity -> World -> World
remove e (World cs as) = World (C.remove @c e cs) as
