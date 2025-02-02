{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.View where

import Data.Aztecs.Entity (ComponentIds, Entity, EntityID, EntityT, FromEntity (..), componentIds)
import Data.Aztecs.Query (IsEq, Query (..), QueryState (..), Queryable (..))
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.World (ArchetypeID, World)
import qualified Data.Aztecs.World as W
import Data.Aztecs.World.Archetype (Archetype, Lookup)
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Archetypes (Archetypes)
import qualified Data.Aztecs.World.Archetypes as AS
import Data.Aztecs.World.Components (Components)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data View a = View
  { viewArchetypes :: Map ArchetypeID Archetype,
    viewQuery :: Query a
  }

view :: forall a. (ComponentIds a, Queryable a) => World -> (View a, World)
view w =
  let (v, cs') = view' @a (W.components w) (W.archetypes w)
   in (v, w {W.components = cs'})

view' :: forall a. (ComponentIds a, Queryable a) => Components -> Archetypes -> (View a, Components)
view' cs as =
  let (cIds, cs') = componentIds @a cs
   in ( View
          { viewArchetypes = AS.lookup cIds as,
            viewQuery = query @a
          },
        cs'
      )

unview :: View a -> World -> World
unview v w =
  w
    { W.archetypes =
        foldr
          (\(aId, arch) as -> AS.adjustArchetype aId (const arch) as)
          (W.archetypes w)
          (Map.toList $ viewArchetypes v)
    }

all :: (FromEntity a) => View (EntityT a) -> Components -> [(EntityID, a)]
all v cs = fromMaybe [] $ do
  let qS = runQuery' (viewQuery v) cs
      es = concatMap (fst . queryStateAll qS) (Map.elems $ viewArchetypes v)
  return $ fmap (\(eId, e) -> (eId, fromEntity e)) es

-- | Map over all entities that match this query,
-- storing the resulting components in the @View@.
map ::
  forall i o.
  (Q.Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o) =>
  (i -> o) ->
  View (EntityT i) ->
  Components ->
  ([o], View (EntityT i))
map f v cs =
  let (o, arches) =
        Q.map' @(IsEq (Entity (EntityT i)) (Entity (EntityT o)))
          f
          cs
          ( \_ g arches' ->
              foldr
                ( \(aId, arch) (acc, archAcc) ->
                    let (os, arch') = g arch
                     in (os : acc, Map.insert aId arch' archAcc)
                )
                ([], Map.empty)
                (Map.toList arches')
          )
          (viewArchetypes v)
   in (o, v {viewArchetypes = arches})

lookup ::
  (FromEntity a, Lookup (Entity (EntityT a))) =>
  EntityID ->
  View (EntityT a) ->
  ArchetypeID ->
  Components ->
  Maybe a
lookup eId v aId cs = do
  arch <- Map.lookup aId (viewArchetypes v)
  e <- A.lookup eId cs arch
  return $ fromEntity e
