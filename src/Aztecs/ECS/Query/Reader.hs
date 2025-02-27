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

    -- * Query state
    QueryReaderState (..),
    queryStateAll,

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
import Aztecs.ECS.Query.Reader.Class
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Aztecs.ECS.World.Entities (Entities (..))
import qualified Aztecs.ECS.World.Entities as E
import Control.Arrow
import Control.Category
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (all, id, (.))

-- | Query to read from entities.
newtype QueryReader i o
  = QueryReader {runQueryReader :: Components -> (QueryReaderState i o, Components)}
  deriving (Functor)

instance Applicative (QueryReader i) where
  {-# INLINE pure #-}
  pure a = QueryReader $ \cs -> (pure a, cs)
  {-# INLINE (<*>) #-}
  QueryReader f <*> QueryReader g = QueryReader $ \cs ->
    let !(aQS, cs') = g cs
        !(bQS, cs'') = f cs'
     in (bQS <*> aQS, cs'')

instance Category QueryReader where
  {-# INLINE id #-}
  id = QueryReader $ \cs -> (id, cs)
  {-# INLINE (.) #-}
  (QueryReader f) . (QueryReader g) = QueryReader $ \cs ->
    let !(aQS, cs') = g cs
        !(bQS, cs'') = f cs'
     in (bQS . aQS, cs'')

instance Arrow QueryReader where
  {-# INLINE arr #-}
  arr f = QueryReader $ \cs -> (QueryReaderState mempty (arr f), cs)
  {-# INLINE first #-}
  first (QueryReader f) = QueryReader $ \cs -> let !(qS, cs') = f cs in (first qS, cs')

instance ArrowChoice QueryReader where
  {-# INLINE left #-}
  left (QueryReader f) = QueryReader $ \comps -> let !(qS, comps') = f comps in (left qS, comps')

instance ArrowQueryReader QueryReader where
  {-# INLINE fetch #-}
  fetch :: forall a. (Component a) => QueryReader () a
  fetch = QueryReader $ \cs ->
    let !(cId, cs') = CS.insert @a cs in (QueryReaderState (Set.singleton cId) (fetchDyn cId), cs')
  {-# INLINE fetchMaybe #-}
  fetchMaybe :: forall a. (Component a) => QueryReader () (Maybe a)
  fetchMaybe = QueryReader $ \cs ->
    let !(cId, cs') = CS.insert @a cs in (QueryReaderState (Set.singleton cId) (fetchMaybeDyn cId), cs')

instance ArrowDynamicQueryReader QueryReader where
  {-# INLINE entity #-}
  entity = QueryReader $ \cs -> (QueryReaderState mempty entity, cs)
  {-# INLINE fetchDyn #-}
  fetchDyn cId = QueryReader $ \cs -> (QueryReaderState (Set.singleton cId) (fetchDyn cId), cs)
  {-# INLINE fetchMaybeDyn #-}
  fetchMaybeDyn cId = QueryReader $ \cs -> (QueryReaderState (Set.singleton cId) (fetchMaybeDyn cId), cs)

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

all :: i -> QueryReader i a -> Entities -> ([a], Entities)
all i q es = let !(as, cs) = all' i q es in (as, es {E.components = cs})

-- | Match all entities.
all' :: i -> QueryReader i a -> Entities -> ([a], Components)
all' i q es =
  let !(qs, cs') = runQueryReader q (E.components es) in (queryStateAll i qs es, cs')

-- | State produced by a `QueryReader`.
data QueryReaderState i o = QueryReaderState
  { queryReaderStateReads :: {-# UNPACK #-} !(Set ComponentID),
    queryReaderStateDyn :: !(DynamicQueryReader i o)
  }
  deriving (Functor)

instance Applicative (QueryReaderState i) where
  {-# INLINE pure #-}
  pure a = QueryReaderState mempty (pure a)
  {-# INLINE (<*>) #-}
  QueryReaderState aRWs aQ <*> QueryReaderState bRWs bQ = QueryReaderState (aRWs <> bRWs) (aQ <*> bQ)

instance Category QueryReaderState where
  {-# INLINE id #-}
  id = QueryReaderState mempty id
  {-# INLINE (.) #-}
  QueryReaderState aRWs aQ . QueryReaderState bRWs bQ = QueryReaderState (aRWs <> bRWs) (aQ . bQ)

instance Arrow QueryReaderState where
  {-# INLINE arr #-}
  arr f = QueryReaderState mempty (arr f)
  {-# INLINE first #-}
  first (QueryReaderState rw q) = QueryReaderState rw (first q)

instance ArrowChoice QueryReaderState where
  {-# INLINE left #-}
  left (QueryReaderState rw q) = QueryReaderState rw (left q)

-- | Match all entities.
{-# INLINE queryStateAll #-}
queryStateAll :: i -> QueryReaderState i a -> Entities -> [a]
queryStateAll i q = allDyn (queryReaderStateReads q) i (queryReaderStateDyn q)
