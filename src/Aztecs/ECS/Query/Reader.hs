{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.Query.Reader
  ( -- * Queries
    QueryReader (..),
    ArrowQueryReader (..),
    ArrowDynamicQueryReader (..),

    -- ** Running
    all,
    all',

    -- * Filters
    QueryFilter (..),
    with,
    without,
    DynamicQueryFilter (..),
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic (DynamicQueryFilter (..))
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReader (..), allDyn)
import Aztecs.ECS.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..))
import Aztecs.ECS.Query.Reader.Class (ArrowQueryReader (..))
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Aztecs.ECS.World.Entities (Entities (..))
import qualified Aztecs.ECS.World.Entities as E
import Control.Arrow (Arrow (..), ArrowChoice (..))
import Control.Category (Category (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (all, id, (.))

-- | Query to read from entities.
newtype QueryReader i o
  = QueryReader {runQueryReader :: Components -> (Set ComponentID, Components, DynamicQueryReader i o)}
  deriving (Functor)

instance Applicative (QueryReader i) where
  pure a = QueryReader $ \cs -> (mempty, cs, pure a)
  (QueryReader f) <*> (QueryReader g) = QueryReader $ \cs ->
    let (cIdsG, cs', aQS) = g cs
        (cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS <*> aQS)

instance Category QueryReader where
  id = QueryReader $ \cs -> (mempty, cs, id)
  (QueryReader f) . (QueryReader g) = QueryReader $ \cs ->
    let (cIdsG, cs', aQS) = g cs
        (cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS . aQS)

instance Arrow QueryReader where
  arr f = QueryReader $ \cs -> (mempty, cs, arr f)
  first (QueryReader f) = QueryReader $ \comps -> let (cIds, comps', qS) = f comps in (cIds, comps', first qS)

instance ArrowChoice QueryReader where
  left (QueryReader f) = QueryReader $ \comps -> let (cIds, comps', qS) = f comps in (cIds, comps', left qS)

instance ArrowQueryReader QueryReader where
  fetch :: forall a. (Component a) => QueryReader () a
  fetch = QueryReader $ \cs ->
    let (cId, cs') = CS.insert @a cs in (Set.singleton cId, cs', fetchDyn cId)
  fetchMaybe :: forall a. (Component a) => QueryReader () (Maybe a)
  fetchMaybe = QueryReader $ \cs ->
    let (cId, cs') = CS.insert @a cs in (Set.singleton cId, cs', fetchMaybeDyn cId)

instance ArrowDynamicQueryReader QueryReader where
  entity = QueryReader $ \cs -> (mempty, cs, entity)
  fetchDyn cId = QueryReader $ \cs -> (Set.singleton cId, cs, fetchDyn cId)
  fetchMaybeDyn cId = QueryReader $ \cs -> (Set.singleton cId, cs, fetchMaybeDyn cId)

-- | Filter for a `Query`.
newtype QueryFilter = QueryFilter {runQueryFilter :: Components -> (DynamicQueryFilter, Components)}

instance Semigroup QueryFilter where
  a <> b =
    QueryFilter
      ( \cs ->
          let (withA', cs') = runQueryFilter a cs
              (withB', cs'') = runQueryFilter b cs'
           in (withA' <> withB', cs'')
      )

instance Monoid QueryFilter where
  mempty = QueryFilter (mempty,)

-- | Filter for entities containing this component.
with :: forall a. (Component a) => QueryFilter
with = QueryFilter $ \cs ->
  let (cId, cs') = CS.insert @a cs in (mempty {filterWith = Set.singleton cId}, cs')

-- | Filter out entities containing this component.
without :: forall a. (Component a) => QueryFilter
without = QueryFilter $ \cs ->
  let (cId, cs') = CS.insert @a cs in (mempty {filterWithout = Set.singleton cId}, cs')

all :: i -> QueryReader i a -> Entities -> ([a], Entities)
all i q es = let (as, cs) = all' i q es in (as, es {E.components = cs})

-- | Match all entities.
all' :: i -> QueryReader i a -> Entities -> ([a], Components)
all' i q es = let (rs, cs', dynQ) = runQueryReader q (E.components es) in (allDyn rs i dynQ es, cs')
