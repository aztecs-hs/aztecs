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

-- | Query for matching entities.
--
-- === Do notation:
-- > move :: (ArrowQuery arr) => arr () Position
-- > move = proc () -> do
-- >   Velocity v <- Q.fetch -< ()
-- >   Position p <- Q.fetch -< ()
-- >   Q.set -< Position $ p + v
--
-- === Arrow combinators:
-- > move :: (ArrowQuery arr) => arr () Position
-- > move = Q.fetch &&& Q.fetch >>> arr (\(Position p, Velocity v) -> Position $ p + v) >>> Q.set
--
-- === Applicative combinators:
-- > move :: (ArrowQuery arr) => arr () Position
-- > move = (,) <$> Q.fetch <*> Q.fetch >>> arr (\(Position p, Velocity v) -> Position $ p + v) >>> Q.set
module Aztecs.ECS.Query
  ( -- * Queries
    Query,
    QueryT (..),
    ArrowQueryReader (..),
    ArrowQuery (..),
    ArrowDynamicQueryReader (..),
    ArrowDynamicQuery (..),

    -- ** Running
    all,
    all',
    single,
    single',
    singleMaybe,
    singleMaybe',
    map,
    mapSingle,
    mapSingleMaybe,

    -- ** Conversion
    fromReader,
    toReader,

    -- * Filters
    QueryFilter (..),
    with,
    without,

    -- * Reads and writes
    ReadsWrites (..),
    disjoint,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Class (ArrowQuery (..))
import Aztecs.ECS.Query.Dynamic (DynamicQueryT, fromDynReader, mapDyn, mapSingleDyn, mapSingleMaybeDyn, toDynReader)
import Aztecs.ECS.Query.Dynamic.Class (ArrowDynamicQuery (..))
import Aztecs.ECS.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..))
import Aztecs.ECS.Query.Reader (QueryFilter (..), QueryReaderT (..), with, without)
import qualified Aztecs.ECS.Query.Reader as QR
import Aztecs.ECS.Query.Reader.Class (ArrowQueryReader (..))
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Arrow (Arrow (..), ArrowChoice (..))
import Control.Category (Category (..))
import Control.Monad.Identity
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack
import Prelude hiding (all, id, map, reads, (.))

type Query i = QueryT Identity i

newtype QueryT m i o = Query {runQuery :: Components -> (ReadsWrites, Components, DynamicQueryT m i o)}
  deriving (Functor)

instance (Monad m) => Applicative (QueryT m i) where
  {-# INLINE pure #-}
  pure a = Query (mempty,,pure a)
  {-# INLINE (<*>) #-}
  (Query f) <*> (Query g) = Query $ \cs ->
    let !(cIdsG, cs', aQS) = g cs
        !(cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS <*> aQS)

instance (Monad m) => Category (QueryT m) where
  {-# INLINE id #-}
  id = Query (mempty,,id)
  {-# INLINE (.) #-}
  (Query f) . (Query g) = Query $ \cs ->
    let !(cIdsG, cs', aQS) = g cs
        !(cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS . aQS)

instance (Monad m) => Arrow (QueryT m) where
  {-# INLINE arr #-}
  arr f = Query (mempty,,arr f)
  {-# INLINE first #-}
  first (Query f) = Query $ \cs -> let !(cIds, cs', qS) = f cs in (cIds, cs', first qS)

instance (Monad m) => ArrowChoice (QueryT m) where
  {-# INLINE left #-}
  left (Query f) = Query $ \cs -> let !(cIds, cs', qS) = f cs in (cIds, cs', left qS)

instance (Monad m) => ArrowQueryReader (QueryT m) where
  {-# INLINE fetch #-}
  fetch = fromReader fetch
  {-# INLINE fetchMaybe #-}
  fetchMaybe = fromReader fetchMaybe

instance (Monad m) => ArrowDynamicQueryReader (QueryT m) where
  {-# INLINE entity #-}
  entity = fromReader entity
  {-# INLINE fetchDyn #-}
  fetchDyn = fromReader . fetchDyn
  {-# INLINE fetchMaybeDyn #-}
  fetchMaybeDyn = fromReader . fetchMaybeDyn

instance (Monad m) => ArrowDynamicQuery m (QueryT m) where
  {-# INLINE adjustDyn #-}
  adjustDyn f cId = Query (ReadsWrites Set.empty (Set.singleton cId),,adjustDyn f cId)
  {-# INLINE adjustDyn_ #-}
  adjustDyn_ f cId = Query (ReadsWrites Set.empty (Set.singleton cId),,adjustDyn_ f cId)
  {-# INLINE adjustDynM #-}
  adjustDynM f cId = Query (ReadsWrites Set.empty (Set.singleton cId),,adjustDynM f cId)
  {-# INLINE setDyn #-}
  setDyn cId = Query (ReadsWrites Set.empty (Set.singleton cId),,setDyn cId)

instance (Monad m) => ArrowQuery m (QueryT m) where
  {-# INLINE adjust #-}
  adjust :: forall i a. (Component a) => (i -> a -> a) -> QueryT m i a
  adjust f = fromDyn @a $ adjustDyn f

  {-# INLINE adjust_ #-}
  adjust_ :: forall i a. (Component a) => (i -> a -> a) -> QueryT m i ()
  adjust_ f = fromDyn @a $ adjustDyn_ f

  {-# INLINE adjustM #-}
  adjustM :: forall i a. (Component a, Monad m) => (i -> a -> m a) -> QueryT m i a
  adjustM f = fromDyn @a $ adjustDynM f

  {-# INLINE set #-}
  set :: forall a. (Component a) => QueryT m a a
  set = fromDyn @a $ setDyn

{-# INLINE fromDyn #-}
fromDyn :: forall a m i o. (Component a) => (ComponentID -> DynamicQueryT m i o) -> QueryT m i o
fromDyn f = Query $ \cs ->
  let !(cId, cs') = CS.insert @a cs in (ReadsWrites (Set.singleton cId) (Set.singleton cId), cs', f cId)

{-# INLINE fromReader #-}
fromReader :: (Monad m) => QueryReaderT m i o -> QueryT m i o
fromReader (QueryReader f) = Query $ \cs ->
  let !(cIds, cs', dynQ) = f cs in (ReadsWrites cIds Set.empty, cs', fromDynReader dynQ)

{-# INLINE toReader #-}
toReader :: (Functor m) => QueryT m i o -> QueryReaderT m i o
toReader (Query f) = QueryReader $ \cs ->
  let !(rws, cs', dynQ) = f cs in (reads rws, cs', toDynReader dynQ)

-- | Reads and writes of a `Query`.
data ReadsWrites = ReadsWrites
  { reads :: !(Set ComponentID),
    writes :: !(Set ComponentID)
  }
  deriving (Show)

instance Semigroup ReadsWrites where
  ReadsWrites r1 w1 <> ReadsWrites r2 w2 = ReadsWrites (r1 <> r2) (w1 <> w2)

instance Monoid ReadsWrites where
  mempty = ReadsWrites mempty mempty

-- | `True` if the reads and writes of two `Query`s overlap.
disjoint :: ReadsWrites -> ReadsWrites -> Bool
disjoint a b =
  Set.disjoint (reads a) (writes b)
    || Set.disjoint (reads b) (writes a)
    || Set.disjoint (writes b) (writes a)

-- | Match all entities.
{-# INLINE all #-}
all :: i -> Query i a -> Entities -> ([a], Entities)
all i = QR.all i . toReader

-- | Match all entities.
{-# INLINE all' #-}
all' :: i -> Query i a -> Entities -> ([a], Components)
all' i = QR.all' i . toReader

-- | Match a single entity.
{-# INLINE single #-}
single :: (HasCallStack) => i -> Query i a -> Entities -> (a, Entities)
single i = QR.single i . toReader

-- | Match a single entity.
{-# INLINE single' #-}
single' :: (HasCallStack) => i -> Query i a -> Entities -> (a, Components)
single' i = QR.single' i . toReader

-- | Match a single entity, or `Nothing`.
{-# INLINE singleMaybe #-}
singleMaybe :: i -> Query i a -> Entities -> (Maybe a, Entities)
singleMaybe i = QR.singleMaybe i . toReader

-- | Match a single entity, or `Nothing`.
{-# INLINE singleMaybe' #-}
singleMaybe' :: i -> Query i a -> Entities -> (Maybe a, Components)
singleMaybe' i = QR.singleMaybe' i . toReader

-- | Map all matched entities.
{-# INLINE map #-}
map :: (Monad m) => i -> QueryT m i o -> Entities -> m ([o], Entities)
map i q es = do
  let !(rws, cs', dynQ) = runQuery q $ components es
      !cIds = reads rws <> writes rws
  (as, es') <- mapDyn cIds i dynQ es
  return (as, es' {components = cs'})

{-# INLINE mapSingle #-}
mapSingle :: (HasCallStack, Monad m) => i -> QueryT m i a -> Entities -> m (a, Entities)
mapSingle i q es = do
  let !(rws, cs', dynQ) = runQuery q $ components es
      !cIds = reads rws <> writes rws
  (as, es') <- mapSingleDyn cIds i dynQ es
  return (as, es' {components = cs'})

{-# INLINE mapSingleMaybe #-}
mapSingleMaybe :: (Monad m) => i -> QueryT m i a -> Entities -> m (Maybe a, Entities)
mapSingleMaybe i q es = do
  let !(rws, cs', dynQ) = runQuery q $ components es
      !cIds = reads rws <> writes rws
  (as, es') <- mapSingleMaybeDyn cIds i dynQ es
  return (as, es' {components = cs'})
