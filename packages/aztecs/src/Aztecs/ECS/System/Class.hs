{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System.Class (ArrowSystem (..)) where

import Aztecs.ECS.Access (Access)
import Aztecs.ECS.Query (Query (..), ReadsWrites)
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.Query.Reader (QueryFilter (..), filterWith, filterWithout)
import Aztecs.ECS.System.Dynamic.Class (filterMapDyn', mapDyn', queueDyn')
import Aztecs.ECS.System.Reader.Class (ArrowReaderSystem)
import Aztecs.ECS.View (View)
import Aztecs.ECS.World (World)
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Components (Components)
import Control.Arrow (Arrow (..), (>>>))
import qualified Data.Foldable as F
import Prelude hiding (map)

class (ArrowReaderSystem arr) => ArrowSystem arr where
  -- | Set a `Component` by its type.
  runArrowSystem :: (Components -> (World -> i -> (o, View, Access ()), ReadsWrites, Components)) -> arr i o

  -- | Query and update all matching entities.
  map :: (ArrowSystem arr) => Query i a -> arr i [a]
  map q = runArrowSystem $ \cs ->
    let !(rws, cs', dynQ) = runQuery q cs
     in (mapDyn' (Q.reads rws <> Q.writes rws) dynQ, rws, cs')

  -- | Query and update all matching entities, ignoring the results.
  map_ :: (ArrowSystem arr) => Query i o -> arr i ()
  map_ q = map q >>> arr (const ())

  -- | Map all matching entities with a `QueryFilter`, storing the updated entities.
  filterMap :: (ArrowSystem arr) => Query i a -> QueryFilter -> arr i [a]
  filterMap q qf = runArrowSystem $ \cs ->
    let !(rws, cs', dynQ) = runQuery q cs
        !(dynQf, cs'') = runQueryFilter qf cs'
        f' n =
          F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynQf)
            && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynQf)
     in (filterMapDyn' (Q.reads rws <> Q.writes rws) dynQ f', rws, cs'')

  -- | Map a single matching entity, storing the updated components.
  -- If there are zero or multiple matching entities, an error will be thrown.
  mapSingle :: (ArrowSystem arr) => Query i a -> arr i a
  mapSingle q =
    map q
      >>> arr
        ( \as -> case as of
            [a] -> a
            _ -> error "TODO"
        )

  -- | Queue an `Access` to happen after this system schedule.
  queue :: (ArrowSystem arr) => (i -> Access ()) -> arr i ()
  queue f = runArrowSystem $ \cs -> (queueDyn' f, mempty, cs)
