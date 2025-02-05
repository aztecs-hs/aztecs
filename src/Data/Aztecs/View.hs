{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Data.Aztecs.World.Archetype (Archetype)
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Archetypes (ArchetypeID, Archetypes)
import qualified Data.Aztecs.World.Archetypes as AS
import Data.Aztecs.World.Components (ComponentID)
import Data.Foldable (foldrM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

-- | View into a `World`, containing a subset of archetypes.
newtype View = View {viewArchetypes :: Map ArchetypeID Archetype}
  deriving (Show, Semigroup, Monoid)

-- | View into all archetypes containing the provided component IDs.
view :: Set ComponentID -> Archetypes -> View
view cIds as = View $ AS.lookup cIds as

-- | View into all archetypes containing the provided component IDs and matching the provided predicate.
filterView ::
  Set ComponentID ->
  (Archetype -> Bool) ->
  Archetypes ->
  View
filterView cIds f as = View $ Map.filter f (AS.lookup cIds as)

-- | "Un-view" a `View` back into a `World`.
unview :: View -> World -> World
unview v w =
  w
    { W.archetypes =
        foldr
          (\(aId, arch) as -> AS.adjustArchetype aId (const arch) as)
          (W.archetypes w)
          (Map.toList $ viewArchetypes v)
    }

-- | Query all matching entities in a `View`.
allDyn :: (Monad m) => DynamicQuery m () a -> View -> m ([a], View)
allDyn q v =
  fmap (\(as, arches) -> (as, View arches)) $
    foldrM
      ( \(aId, arch) (acc, archAcc) -> do
          (as, arch') <- dynQueryAll q (repeat ()) (A.entities arch) arch
          return (as ++ acc, Map.insert aId arch' archAcc)
      )
      ([], Map.empty)
      (Map.toList $ viewArchetypes v)
