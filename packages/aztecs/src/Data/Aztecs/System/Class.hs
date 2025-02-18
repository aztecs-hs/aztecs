{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aztecs.System.Class
  ( ArrowSystem (..),
    map,
    map_,
    filterMap,
    mapSingle,
    queue,
  )
where

import Control.Arrow (Arrow (..), (>>>))
import Data.Aztecs.Access (Access)
import Data.Aztecs.Query (Query (..), ReadsWrites)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.Query.Reader (QueryFilter (..), filterWith, filterWithout)
import Data.Aztecs.System.Dynamic.Class (filterMapDyn', mapDyn', queueDyn')
import Data.Aztecs.System.Reader.Class (ArrowSystemReader)
import Data.Aztecs.View (View)
import Data.Aztecs.World (World)
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Archetypes (Node (..))
import Data.Aztecs.World.Components (Components)
import qualified Data.Foldable as F
import Prelude hiding (map)

class (ArrowSystemReader m arr) => ArrowSystem m arr where
  -- | Set a `Component` by its type.
  runArrowSystem :: (Components -> (World -> i -> m (o, View, Access m ()), ReadsWrites, Components)) -> arr i o

-- | Query and update all matching entities.
map :: (ArrowSystem m arr) => Query m i a -> arr i [a]
map q = runArrowSystem $ \cs ->
  let !(rws, cs', dynQ) = runQuery q cs
   in (mapDyn' (Q.reads rws <> Q.writes rws) dynQ, rws, cs')

-- | Query and update all matching entities, ignoring the results.
map_ :: (ArrowSystem m arr) => Query m i o -> arr i ()
map_ q = map q >>> arr (const ())

-- | Map all matching entities with a `QueryFilter`, storing the updated entities.
filterMap :: (ArrowSystem m arr) => Query m i a -> QueryFilter -> arr i [a]
filterMap q qf = runArrowSystem $ \cs ->
  let !(rws, cs', dynQ) = runQuery q cs
      !(dynQf, cs'') = runQueryFilter qf cs'
      f' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynQf)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynQf)
   in (filterMapDyn' (Q.reads rws <> Q.writes rws) dynQ f', rws, cs'')

-- | Map a single matching entity, storing the updated components.
-- If there are zero or multiple matching entities, an error will be thrown.
mapSingle :: (ArrowSystem m arr) => Query m i a -> arr i a
mapSingle q =
  (map q)
    >>> arr
      ( \as -> case as of
          [a] -> a
          _ -> error "TODO"
      )

-- | Queue an `Access` to happen after this system schedule.
queue :: (ArrowSystem m arr) => (i -> Access m ()) -> arr i ()
queue f = runArrowSystem $ \cs -> (queueDyn' f, mempty, cs)
