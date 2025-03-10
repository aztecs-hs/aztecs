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
    null,
    unview,
    allDyn,
    singleDyn,
    mapDyn,
    mapSingleDyn,
  )
where

import Aztecs.ECS.Query.Dynamic
import Aztecs.ECS.World.Archetypes
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Entities (Entities)
import qualified Aztecs.ECS.World.Entities as E
import Data.Foldable (foldl', foldlM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Prelude hiding (null)

-- | View into a `World`, containing a subset of archetypes.
--
-- @since 0.9
newtype View = View
  { -- | Archetypes contained in this view.
    --
    -- @since 0.9
    viewArchetypes :: Map ArchetypeID Node
  }
  deriving (Show, Semigroup, Monoid)

-- | View into all archetypes containing the provided component IDs.
--
-- @since 0.9
view :: QueryFilter -> Archetypes -> View
view qf as = View $ AS.find (filterWith qf) (filterWithout qf) as

-- | View into a single archetype containing the provided component IDs.
--
-- @since 0.9
viewSingle :: QueryFilter -> Archetypes -> Maybe View
viewSingle qf as = case Map.toList $ AS.find (filterWith qf) (filterWithout qf) as of
  [a] -> Just . View $ uncurry Map.singleton a
  _ -> Nothing

-- | @True@ if the `View` is empty.
--
-- @since 0.9
null :: View -> Bool
null = Map.null . viewArchetypes

-- | "Un-view" a `View` back into a `World`.
--
-- @since 0.9
unview :: View -> Entities -> Entities
unview v es =
  es
    { E.archetypes =
        foldl'
          (\as (aId, n) -> as {AS.nodes = Map.insert aId n (AS.nodes as)})
          (E.archetypes es)
          (Map.toList $ viewArchetypes v)
    }

-- | Query all matching entities in a `View`.
--
-- @since 0.9
allDyn :: (Monad m) => DynamicQueryT m a -> View -> m [a]
allDyn q v =
  foldlM
    ( \acc n -> do
        as <- readDynQuery q $ nodeArchetype n
        return $ as ++ acc
    )
    []
    (viewArchetypes v)

-- | Query all matching entities in a `View`.
--
-- @since 0.9
singleDyn :: (Monad m) => DynamicQueryT m a -> View -> m (Maybe a)
singleDyn q v = do
  as <- allDyn q v
  return $ case as of
    [a] -> Just a
    _ -> Nothing

-- | Map all matching entities in a `View`.
--
-- @since 0.9
mapDyn :: (Monad m) => DynamicQueryT m a -> View -> m ([a], View)
mapDyn q v = do
  (as, arches) <-
    foldlM
      ( \(acc, archAcc) (aId, n) -> do
          (as', arch') <- runDynQuery q $ nodeArchetype n
          return (as' ++ acc, Map.insert aId (n {nodeArchetype = arch'}) archAcc)
      )
      ([], Map.empty)
      (Map.toList $ viewArchetypes v)
  return (as, View arches)

-- | Map a single matching entity in a `View`.
--
-- @since 0.9
mapSingleDyn :: (Monad m) => DynamicQueryT m a -> View -> m (Maybe a, View)
mapSingleDyn q v = do
  (as, arches) <- mapDyn q v
  return $ case as of
    [a] -> (Just a, arches)
    _ -> (Nothing, arches)
