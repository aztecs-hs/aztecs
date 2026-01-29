{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Aztecs.ECS.View
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.View
  ( View (..),
    view,
    viewSingle,
    filterView,
    null,
    unview,
    allDyn,
    singleDyn,
    mapDyn,
    mapSingleDyn,
  )
where

import Aztecs.ECS.Access.Internal (Access)
import Aztecs.ECS.Query.Dynamic (DynamicQuery (..))
import Aztecs.ECS.World.Archetypes
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Components
import Aztecs.ECS.World.Entities (Entities)
import qualified Aztecs.ECS.World.Entities as E
import Control.Monad.Identity (Identity (runIdentity))
import Data.Foldable hiding (null)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Prelude hiding (null)

-- | View into a `World`, containing a subset of archetypes.
newtype View m = View
  { -- | Archetypes contained in this view.
    viewArchetypes :: Map ArchetypeID (Node m)
  }
  deriving (Show, Semigroup, Monoid)

-- | View into all archetypes containing the provided component IDs.
view :: Set ComponentID -> Archetypes m -> View m
view cIds as = View $ AS.find cIds as

-- | View into a single archetype containing the provided component IDs.
viewSingle :: Set ComponentID -> Archetypes m -> Maybe (View m)
viewSingle cIds as = case Map.toList $ AS.find cIds as of
  [a] -> Just . View $ uncurry Map.singleton a
  _ -> Nothing

-- | View into all archetypes containing the provided component IDs and matching the provided predicate.
filterView ::
  Set ComponentID ->
  (Node m -> Bool) ->
  Archetypes m ->
  View m
filterView cIds f as = View $ Map.filter f (AS.find cIds as)

-- | @True@ if the `View` is empty.
null :: View m -> Bool
null = Map.null . viewArchetypes

-- | "Un-view" a `View` back into a `World`.
unview :: View m -> Entities m -> Entities m
unview v es =
  es
    { E.archetypes =
        foldl'
          (\as (aId, n) -> as {AS.nodes = Map.insert aId n (AS.nodes as)})
          (E.archetypes es)
          (Map.toList $ viewArchetypes v)
    }

-- | Query all matching entities in a `View`.
allDyn :: DynamicQuery Identity a -> View Identity -> [a]
allDyn q v =
  foldl'
    ( \acc n ->
        let (as, _, _) = runIdentity . runDynQuery q $ nodeArchetype n
         in as ++ acc
    )
    []
    (viewArchetypes v)

-- | Query all matching entities in a `View`.
singleDyn :: DynamicQuery Identity a -> View Identity -> Maybe a
singleDyn q v = case allDyn q v of
  [x] -> Just x
  _ -> Nothing

-- | Map all matching entities in a `View`. Returns the results, updated view, and hooks to run.
mapDyn :: (Monad m) => DynamicQuery m a -> View m -> m ([a], View m, Access m ())
mapDyn q v = do
  (as, arches, hooks) <-
    foldlM
      ( \(acc, archAcc, hooksAcc) (aId, n) -> do
          (as', arch', hook) <- runDynQuery q $ nodeArchetype n
          return (as' ++ acc, Map.insert aId (n {nodeArchetype = arch'}) archAcc, hooksAcc >> hook)
      )
      ([], Map.empty, return ())
      (Map.toList $ viewArchetypes v)
  return (as, View arches, hooks)

-- | Map a single matching entity in a `View`. Returns the result, updated view, and hooks to run.
mapSingleDyn :: (Monad m) => DynamicQuery m a -> View m -> m (Maybe a, View m, Access m ())
mapSingleDyn q v = do
  (as, arches, hooks) <- mapDyn q v
  return $ case as of
    [x] -> (Just x, arches, hooks)
    _ -> (Nothing, arches, hooks)
