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

import Aztecs.ECS.Query.Dynamic (DynamicQuery, DynamicQueryT (..))
import Aztecs.ECS.World.Archetypes
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Components
import Aztecs.ECS.World.Entities (Entities)
import qualified Aztecs.ECS.World.Entities as E
import Control.Monad.Identity (Identity (runIdentity))
import Data.Foldable (foldlM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Vector as V
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
view :: Set ComponentID -> Archetypes -> View
view cIds as = View $ AS.find cIds as

-- | View into a single archetype containing the provided component IDs.
--
-- @since 0.9
viewSingle :: Set ComponentID -> Archetypes -> Maybe View
viewSingle cIds as = case Map.toList $ AS.find cIds as of
  [a] -> Just . View $ uncurry Map.singleton a
  _ -> Nothing

-- | View into all archetypes containing the provided component IDs and matching the provided predicate.
--
-- @since 0.9
filterView ::
  Set ComponentID ->
  (Node -> Bool) ->
  Archetypes ->
  View
filterView cIds f as = View $ Map.filter f (AS.find cIds as)

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
allDyn :: DynamicQuery a -> View -> Vector a
allDyn q v =
  foldl'
    ( \acc n ->
        let (as, _) = runIdentity . runDynQueryT q $ nodeArchetype n
         in as V.++ acc
    )
    V.empty
    (viewArchetypes v)

-- | Query all matching entities in a `View`.
--
-- @since 0.9
singleDyn :: DynamicQuery a -> View -> Maybe a
singleDyn q v = case allDyn q v of
  as | V.length as == 1 -> Just (V.head as)
  _ -> Nothing

-- | Map all matching entities in a `View`.
--
-- @since 0.9
mapDyn :: (Monad m) => DynamicQueryT m a -> View -> m (Vector a, View)
mapDyn q v = do
  (as, arches) <-
    foldlM
      ( \(acc, archAcc) (aId, n) -> do
          (as', arch') <- runDynQueryT q $ nodeArchetype n
          return (as' V.++ acc, Map.insert aId (n {nodeArchetype = arch'}) archAcc)
      )
      (V.empty, Map.empty)
      (Map.toList $ viewArchetypes v)
  return (as, View arches)

-- | Map a single matching entity in a `View`.
--
-- @since 0.9
mapSingleDyn :: (Monad m) => DynamicQueryT m a -> View -> m (Maybe a, View)
mapSingleDyn q v = do
  (as, arches) <- mapDyn q v
  return $ case as of
    a | V.length a == 1 -> (Just (V.head a), arches)
    _ -> (Nothing, arches)
