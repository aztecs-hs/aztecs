{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (ArchetypeID, Archetypes, Node (..))
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Components (ComponentID)
import Aztecs.ECS.World.Entities (Entities)
import qualified Aztecs.ECS.World.Entities as E
import Data.Foldable (foldl', foldlM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (null)

-- | View into a `World`, containing a subset of archetypes.
newtype View = View {viewArchetypes :: Map ArchetypeID Node}
  deriving (Show, Semigroup, Monoid)

-- | View into all archetypes containing the provided component IDs.
view :: Set ComponentID -> Archetypes -> View
view cIds as = View $ AS.find cIds as

viewSingle :: Set ComponentID -> Archetypes -> Maybe View
viewSingle cIds as = case Map.toList $ AS.find cIds as of
  [a] -> Just . View $ uncurry Map.singleton a
  _ -> Nothing

-- | View into all archetypes containing the provided component IDs and matching the provided predicate.
filterView ::
  Set ComponentID ->
  (Node -> Bool) ->
  Archetypes ->
  View
filterView cIds f as = View $ Map.filter f (AS.find cIds as)

null :: View -> Bool
null = Map.null . viewArchetypes

-- | "Un-view" a `View` back into a `World`.
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
allDyn :: (Monad m) => i -> DynamicQueryReaderT m i a -> View -> m [a]
allDyn i q v =
  foldlM
    ( \acc n -> do
        as <- runDynQueryReaderT i q (Set.toList . A.entities $ nodeArchetype n) (nodeArchetype n)
        return $ as ++ acc
    )
    []
    (viewArchetypes v)

-- | Query all matching entities in a `View`.
singleDyn :: (Monad m) => i -> DynamicQueryReaderT m i a -> View -> m (Maybe a)
singleDyn i q v = do
  as <- allDyn i q v
  return $ case as of
    [a] -> Just a
    _ -> Nothing

-- | Map all matching entities in a `View`.
mapDyn :: (Monad m) => i -> DynamicQueryT m i a -> View -> m ([a], View)
mapDyn i q v = do
  (as, arches) <-
    foldlM
      ( \(acc, archAcc) (aId, n) -> do
          (as', arch') <- runDynQuery q (repeat i) (Set.toList . A.entities $ nodeArchetype n) (nodeArchetype n)
          return (as' ++ acc, Map.insert aId (n {nodeArchetype = arch'}) archAcc)
      )
      ([], Map.empty)
      (Map.toList $ viewArchetypes v)
  return (as, View arches)

mapSingleDyn :: (Monad m) => i -> DynamicQueryT m i a -> View -> m (Maybe a, View)
mapSingleDyn i q v = do
  (as, arches) <- mapDyn i q v
  return $ case as of
    [a] -> (Just a, arches)
    _ -> (Nothing, arches)
