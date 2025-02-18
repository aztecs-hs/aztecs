{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aztecs.System.Reader.Class
  ( ArrowSystemReader (..),
    all,
    filter,
    single,
  )
where

import Control.Arrow (Arrow (..), (>>>))
import Data.Aztecs.Component
import Data.Aztecs.Query.Reader (DynamicQueryFilter (..), QueryFilter (..), QueryReader (..))
import Data.Aztecs.System.Dynamic.Reader.Class (allDyn', filterDyn')
import Data.Aztecs.World (World)
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Archetypes (Node (..))
import Data.Aztecs.World.Components (Components)
import qualified Data.Foldable as F
import Data.Set (Set)
import Prelude hiding (all, any, filter, id, lookup, map, mapM, reads, (.))

class (Arrow arr) => ArrowSystemReader arr where
  -- | Set a `Component` by its type.
  runArrowSystemReader :: (Components -> ((World -> i -> o), Set ComponentID, Components)) -> arr i o

-- | Query all matching entities.
all :: (ArrowSystemReader arr) => QueryReader i a -> arr i [a]
all q = runArrowSystemReader $ \cs ->
  let !(rs, cs', dynQ) = runQueryReader q cs
   in (allDyn' rs dynQ, rs, cs')

-- | Query all matching entities with a `QueryFilter`.
filter :: (ArrowSystemReader arr) => QueryReader () a -> QueryFilter -> arr () [a]
filter q qf = runArrowSystemReader $ \cs ->
  let !(rs, cs', dynQ) = runQueryReader q cs
      !(dynQf, cs'') = runQueryFilter qf cs'
      qf' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynQf)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynQf)
   in (filterDyn' rs dynQ qf', rs, cs'')

-- | Query a single matching entity.
-- If there are zero or multiple matching entities, an error will be thrown.
single :: (ArrowSystemReader arr) => QueryReader () a -> arr () a
single q =
  (all q)
    >>> arr
      ( \as -> case as of
          [a] -> a
          _ -> error "TODO"
      )
