{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.View where

import Data.Aztecs.Query (QueryState (..))
import Data.Aztecs.World (ArchetypeID, World)
import qualified Data.Aztecs.World as W
import Data.Aztecs.World.Archetype (Archetype)
import Data.Aztecs.World.Archetypes (Archetypes)
import qualified Data.Aztecs.World.Archetypes as AS
import Data.Aztecs.World.Components (ComponentID)
import Data.Foldable (foldrM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

data View = View {viewArchetypes :: Map ArchetypeID Archetype}
  deriving (Show)

view :: Set ComponentID -> Archetypes -> View
view cIds as = View $ AS.lookup cIds as

viewFilter ::
  Set ComponentID ->
  (Archetype -> Bool) ->
  Archetypes ->
  View
viewFilter cIds f as = View $ Map.filter f (AS.lookup cIds as)

unview :: View -> World -> World
unview v w =
  w
    { W.archetypes =
        foldr
          (\(aId, arch) as -> AS.adjustArchetype aId (const arch) as)
          (W.archetypes w)
          (Map.toList $ viewArchetypes v)
    }

allState :: (Monad m) => QueryState m () a -> View -> m ([a], View)
allState q v =
  fmap (\(as, arches) -> (as, View arches)) $
    foldrM
      ( \(aId, arch) (acc, archAcc) -> do
          (as, arch') <- queryStateAll q [] arch
          return (as ++ acc, Map.insert aId arch' archAcc)
      )
      ([], Map.empty)
      (Map.toList $ viewArchetypes v)
