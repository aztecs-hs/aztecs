{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.View
  ( View (..),
    view,
    filterView,
    unview,
    allDyn,
  )
where

import Data.Aztecs.Query (DynamicQuery (..))
import Data.Aztecs.World (World)
import qualified Data.Aztecs.World as W
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Archetypes (ArchetypeID, Archetypes, Node (..))
import qualified Data.Aztecs.World.Archetypes as AS
import Data.Aztecs.World.Components (ComponentID)
import Data.Foldable (foldrM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

-- | View into a `World`, containing a subset of archetypes.
newtype View = View {viewArchetypes :: Map ArchetypeID Node}
  deriving (Show, Semigroup, Monoid)

-- | View into all archetypes containing the provided component IDs.
view :: Set ComponentID -> Archetypes -> View
view cIds as = View $ AS.lookup cIds as

-- | View into all archetypes containing the provided component IDs and matching the provided predicate.
filterView ::
  Set ComponentID ->
  (Node -> Bool) ->
  Archetypes ->
  View
filterView cIds f as = View $ Map.filter f (AS.lookup cIds as)

-- | "Un-view" a `View` back into a `World`.
unview :: View -> World -> World
unview v w =
  w
    { W.archetypes =
        foldr
          (\(aId, n) as -> as {AS.nodes = Map.insert aId n (AS.nodes as)})
          (W.archetypes w)
          (Map.toList $ viewArchetypes v)
    }

-- | Query all matching entities in a `View`.
allDyn :: (Monad m) => i -> DynamicQuery m i a -> View -> m ([a], View)
allDyn i q v =
  fmap (\(as, arches) -> (as, View arches)) $
    foldrM
      ( \(aId, n) (acc, archAcc) -> do
          (as, arch') <- dynQueryAll q (repeat i) (A.entities (nodeArchetype n)) (nodeArchetype n)
          return (as ++ acc, Map.insert aId (n {nodeArchetype = arch'}) archAcc)
      )
      ([], Map.empty)
      (Map.toList $ viewArchetypes v)
