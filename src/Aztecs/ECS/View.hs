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

import Aztecs.ECS.Query.Dynamic (DynamicQueryT (..))
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReaderT (..), runDynQueryReaderT)
import Aztecs.ECS.World.Archetypes
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Components
import Aztecs.ECS.World.Entities (Entities)
import qualified Aztecs.ECS.World.Entities as E
import Data.Foldable (foldl', foldlM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Prelude hiding (null)

-- | View into a `World`, containing a subset of archetypes.
--
-- @since 9.0
newtype View = View
  { -- | Archetypes contained in this view.
    --
    -- @since 9.0
    viewArchetypes :: Map ArchetypeID Node
  }
  deriving (Show, Semigroup, Monoid)

-- | View into all archetypes containing the provided component IDs.
--
-- @since 9.0
view :: Set ComponentID -> Archetypes -> View
view cIds as = View $ AS.find cIds as

-- | View into a single archetype containing the provided component IDs.
--
-- @since 9.0
viewSingle :: Set ComponentID -> Archetypes -> Maybe View
viewSingle cIds as = case Map.toList $ AS.find cIds as of
  [a] -> Just . View $ uncurry Map.singleton a
  _ -> Nothing

-- | View into all archetypes containing the provided component IDs and matching the provided predicate.
--
-- @since 9.0
filterView ::
  Set ComponentID ->
  (Node -> Bool) ->
  Archetypes ->
  View
filterView cIds f as = View $ Map.filter f (AS.find cIds as)

-- | @True@ if the `View` is empty.
--
-- @since 9.0
null :: View -> Bool
null = Map.null . viewArchetypes

-- | "Un-view" a `View` back into a `World`.
--
-- @since 9.0
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
-- @since 9.0
allDyn :: (Monad m) => i -> DynamicQueryReaderT m i a -> View -> m [a]
allDyn i q v =
  foldlM
    ( \acc n -> do
        as <- runDynQueryReaderT i q $ nodeArchetype n
        return $ as ++ acc
    )
    []
    (viewArchetypes v)

-- | Query all matching entities in a `View`.
--
-- @since 9.0
singleDyn :: (Monad m) => i -> DynamicQueryReaderT m i a -> View -> m (Maybe a)
singleDyn i q v = do
  as <- allDyn i q v
  return $ case as of
    [a] -> Just a
    _ -> Nothing

-- | Map all matching entities in a `View`.
--
-- @since 9.0
mapDyn :: (Monad m) => i -> DynamicQueryT m i a -> View -> m ([a], View)
mapDyn i q v = do
  (as, arches) <-
    foldlM
      ( \(acc, archAcc) (aId, n) -> do
          (as', arch') <- runDynQuery q (repeat i) $ nodeArchetype n
          return (as' ++ acc, Map.insert aId (n {nodeArchetype = arch'}) archAcc)
      )
      ([], Map.empty)
      (Map.toList $ viewArchetypes v)
  return (as, View arches)

-- | Map a single matching entity in a `View`.
--
-- @since 9.0
mapSingleDyn :: (Monad m) => i -> DynamicQueryT m i a -> View -> m (Maybe a, View)
mapSingleDyn i q v = do
  (as, arches) <- mapDyn i q v
  return $ case as of
    [a] -> (Just a, arches)
    _ -> (Nothing, arches)
