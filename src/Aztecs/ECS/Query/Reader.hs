{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.Query.Reader
  ( -- * Queries
    QueryReader,
    QueryReaderT (..),
    ArrowQueryReader (..),
    ArrowDynamicQueryReader (..),

    -- ** Running
    all,
    all',
    single,
    single',
    singleMaybe,
    singleMaybe',

    -- * Filters
    QueryFilter (..),
    with,
    without,
    DynamicQueryFilter (..),
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic (DynamicQueryFilter (..))
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReaderT (..), allDyn, singleDyn, singleMaybeDyn)
import Aztecs.ECS.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..))
import Aztecs.ECS.Query.Reader.Class (ArrowQueryReader (..))
import Aztecs.ECS.Task
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Aztecs.ECS.World.Entities (Entities (..))
import qualified Aztecs.ECS.World.Entities as E
import Control.Arrow (Arrow (..), ArrowChoice (..))
import Control.Category (Category (..))
import Control.Monad.Identity
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack
import Prelude hiding (all, id, (.))

type QueryReader = QueryReaderT Identity

-- | Query to read from entities.
newtype QueryReaderT m i o
  = QueryReader {runQueryReader :: Components -> (Set ComponentID, Components, DynamicQueryReaderT m i o)}
  deriving (Functor)

instance (Monad m) => Applicative (QueryReaderT m i) where
  {-# INLINE pure #-}
  pure a = QueryReader (mempty,,pure a)
  {-# INLINE (<*>) #-}
  (QueryReader f) <*> (QueryReader g) = QueryReader $ \cs ->
    let !(cIdsG, cs', aQS) = g cs
        !(cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS <*> aQS)

instance (Monad m) => Category (QueryReaderT m) where
  {-# INLINE id #-}
  id = QueryReader (mempty,,id)
  {-# INLINE (.) #-}
  (QueryReader f) . (QueryReader g) = QueryReader $ \cs ->
    let !(cIdsG, cs', aQS) = g cs
        !(cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS . aQS)

instance (Monad m) => Arrow (QueryReaderT m) where
  {-# INLINE arr #-}
  arr f = QueryReader (mempty,,arr f)
  {-# INLINE first #-}
  first (QueryReader f) = QueryReader $ \cs -> let !(cIds, comps', qS) = f cs in (cIds, comps', first qS)

instance (Monad m) => ArrowChoice (QueryReaderT m) where
  {-# INLINE left #-}
  left (QueryReader f) = QueryReader $ \cs -> let !(cIds, comps', qS) = f cs in (cIds, comps', left qS)

instance (Monad m) => ArrowQueryReader (QueryReaderT m) where
  {-# INLINE fetch #-}
  fetch :: forall a. (Component a) => QueryReaderT m () a
  fetch = QueryReader $ \cs ->
    let !(cId, cs') = CS.insert @a cs in (Set.singleton cId, cs', fetchDyn cId)

  {-# INLINE fetchMaybe #-}
  fetchMaybe :: forall a. (Component a) => QueryReaderT m () (Maybe a)
  fetchMaybe = QueryReader $ \cs ->
    let !(cId, cs') = CS.insert @a cs in (Set.singleton cId, cs', fetchMaybeDyn cId)

instance (Monad m) => ArrowDynamicQueryReader (QueryReaderT m) where
  {-# INLINE entity #-}
  entity = QueryReader (mempty,,entity)
  {-# INLINE fetchDyn #-}
  fetchDyn cId = QueryReader (Set.singleton cId,,fetchDyn cId)
  {-# INLINE fetchMaybeDyn #-}
  fetchMaybeDyn cId = QueryReader (Set.singleton cId,,fetchMaybeDyn cId)

instance (Monad m) => ArrowTask m (QueryReaderT m) where
  task f = QueryReader (mempty,,task f)

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

-- | Match a single entity.
{-# INLINE single #-}
single :: (HasCallStack) => i -> QueryReader i a -> Entities -> (a, Entities)
single i q es = let !(a, cs) = single' i q es in (a, es {E.components = cs})

-- | Match a single entity.
{-# INLINE single' #-}
single' :: (HasCallStack) => i -> QueryReader i a -> Entities -> (a, Components)
single' i q es = let !(rs, cs', dynQ) = runQueryReader q (E.components es) in (singleDyn rs i dynQ es, cs')

-- | Match a single entity.
{-# INLINE singleMaybe #-}
singleMaybe :: i -> QueryReader i a -> Entities -> (Maybe a, Entities)
singleMaybe i q es = let !(a, cs) = singleMaybe' i q es in (a, es {E.components = cs})

-- | Match a single entity.
{-# INLINE singleMaybe' #-}
singleMaybe' :: i -> QueryReader i a -> Entities -> (Maybe a, Components)
singleMaybe' i q es = let !(rs, cs', dynQ) = runQueryReader q (E.components es) in (singleMaybeDyn rs i dynQ es, cs')
