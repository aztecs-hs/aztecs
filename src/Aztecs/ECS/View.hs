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

import Aztecs.ECS.Query.Dynamic (DynamicQuery (..))
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReader (..), runDynQueryReader)
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (ArchetypeID, Archetypes, Node (..))
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Components (ComponentID)
import Aztecs.ECS.World.Entities (Entities)
import qualified Aztecs.ECS.World.Entities as E
import Data.Foldable (foldl')
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
allDyn :: i -> DynamicQueryReader i a -> View -> [a]
allDyn i q v =
  foldl'
    ( \acc n ->
        runDynQueryReader i q (Set.toList . A.entities $ nodeArchetype n) (nodeArchetype n) ++ acc
    )
    []
    (viewArchetypes v)

-- | Query all matching entities in a `View`.
singleDyn :: i -> DynamicQueryReader i a -> View -> Maybe a
singleDyn i q v = case allDyn i q v of
  [a] -> Just a
  _ -> Nothing

-- | Map all matching entities in a `View`.
mapDyn :: i -> DynamicQuery i a -> View -> ([a], View)
mapDyn i q v =
  let (as, arches) =
        foldl'
          ( \(acc, archAcc) (aId, n) ->
              let (as', arch') = runDynQuery q (repeat i) (Set.toList . A.entities $ nodeArchetype n) (nodeArchetype n)
               in (as' ++ acc, Map.insert aId (n {nodeArchetype = arch'}) archAcc)
          )
          ([], Map.empty)
          (Map.toList $ viewArchetypes v)
   in (as, View arches)

mapSingleDyn :: i -> DynamicQuery i a -> View -> (Maybe a, View)
mapSingleDyn i q v =
  let (as, arches) = mapDyn i q v
   in case as of
        [a] -> (Just a, arches)
        _ -> (Nothing, arches)
