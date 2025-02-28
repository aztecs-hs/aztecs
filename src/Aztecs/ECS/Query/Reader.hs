{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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
  {-# INLINE pure #-}
  pure a = QueryReader $ \cs -> (mempty, cs, pure a)
  {-# INLINE (<*>) #-}
  (QueryReader f) <*> (QueryReader g) = QueryReader $ \cs ->
    let !(cIdsG, cs', aQS) = g cs
        !(cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS <*> aQS)

instance Category QueryReader where
  {-# INLINE id #-}
  id = QueryReader $ \cs -> (mempty, cs, id)
  {-# INLINE (.) #-}
  (QueryReader f) . (QueryReader g) = QueryReader $ \cs ->
    let !(cIdsG, cs', aQS) = g cs
        !(cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS . aQS)

instance Arrow QueryReader where
  {-# INLINE arr #-}
  arr f = QueryReader $ \cs -> (mempty, cs, arr f)
  {-# INLINE first #-}
  first (QueryReader f) = QueryReader $ \cs -> let !(cIds, comps', qS) = f cs in (cIds, comps', first qS)

instance ArrowChoice QueryReader where
  {-# INLINE left #-}
  left (QueryReader f) = QueryReader $ \cs -> let !(cIds, comps', qS) = f cs in (cIds, comps', left qS)

instance ArrowQueryReader QueryReader where
  {-# INLINE fetch #-}
  fetch :: forall a. (Component a) => QueryReader () a
  fetch = QueryReader $ \cs ->
    let !(cId, cs') = CS.insert @a cs in (Set.singleton cId, cs', fetchDyn cId)

  {-# INLINE fetchMaybe #-}
  fetchMaybe :: forall a. (Component a) => QueryReader () (Maybe a)
  fetchMaybe = QueryReader $ \cs ->
    let !(cId, cs') = CS.insert @a cs in (Set.singleton cId, cs', fetchMaybeDyn cId)

instance ArrowDynamicQueryReader QueryReader where
  {-# INLINE entity #-}
  entity = QueryReader $ \cs -> (mempty, cs, entity)
  {-# INLINE fetchDyn #-}
  fetchDyn cId = QueryReader $ \cs -> (Set.singleton cId, cs, fetchDyn cId)
  {-# INLINE fetchMaybeDyn #-}
  fetchMaybeDyn cId = QueryReader $ \cs -> (Set.singleton cId, cs, fetchMaybeDyn cId)

-- | Filter for a `Query`.
newtype QueryFilter = QueryFilter {runQueryFilter :: Components -> (DynamicQueryFilter, Components)}

instance Semigroup QueryFilter where
  a <> b =
    QueryFilter
      ( \cs ->
          let !(withA', cs') = runQueryFilter a cs
              !(withB', cs'') = runQueryFilter b cs'
           in (withA' <> withB', cs'')
      )

instance Monoid QueryFilter where
  mempty = QueryFilter (mempty,)

-- | Filter for entities containing this component.
with :: forall a. (Component a) => QueryFilter
with = QueryFilter $ \cs ->
  let !(cId, cs') = CS.insert @a cs in (mempty {filterWith = Set.singleton cId}, cs')

-- | Filter out entities containing this component.
without :: forall a. (Component a) => QueryFilter
without = QueryFilter $ \cs ->
  let !(cId, cs') = CS.insert @a cs in (mempty {filterWithout = Set.singleton cId}, cs')

-- | Match all entities.
{-# INLINE all #-}
all :: i -> QueryReader i a -> Entities -> ([a], Entities)
all i q es = let !(as, cs) = all' i q es in (as, es {E.components = cs})

-- | Match all entities.
{-# INLINE all' #-}
all' :: i -> QueryReader i a -> Entities -> ([a], Components)
all' i q es = let !(rs, cs', dynQ) = runQueryReader q (E.components es) in (allDyn rs i dynQ es, cs')
