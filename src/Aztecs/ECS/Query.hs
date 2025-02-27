{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.Query
  ( -- * Queries
    Query (..),
    ArrowQueryReader (..),
    ArrowQuery (..),
    ArrowDynamicQueryReader (..),
    ArrowDynamicQuery (..),

    -- ** Running
    all,
    map,

    -- ** Conversion
    fromReader,
    toReader,

    -- * Query state
    QueryState (..),

    -- ** Running
    queryStateAll,
    queryStateMap,

    -- ** Conversion
    queryStateFromReader,
    queryStateToReader,

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
import Aztecs.ECS.Query.Class
import Aztecs.ECS.Query.Dynamic
import Aztecs.ECS.Query.Dynamic.Reader (allDyn)
import Aztecs.ECS.Query.Reader (QueryFilter (..), QueryReader (..), QueryReaderState (..), with, without)
import qualified Aztecs.ECS.Query.Reader as QR
import Aztecs.ECS.Query.Reader.Class
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Arrow
import Control.Category
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (all, id, map, reads, (.))

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
newtype Query i o = Query {runQuery :: Components -> (QueryState i o, Components)}
  deriving (Functor)

instance Applicative (Query i) where
  {-# INLINE pure #-}
  pure a = Query (pure a,)
  {-# INLINE (<*>) #-}
  (Query f) <*> (Query g) = Query $ \cs ->
    let !(aQS, cs') = g cs
        !(bQS, cs'') = f cs'
     in (bQS <*> aQS, cs'')

instance Category Query where
  {-# INLINE id #-}
  id = Query (id,)
  {-# INLINE (.) #-}
  (Query f) . (Query g) = Query $ \cs ->
    let !(aQS, cs') = g cs
        !(bQS, cs'') = f cs'
     in (bQS . aQS, cs'')

instance Arrow Query where
  {-# INLINE arr #-}
  arr f = Query (arr f,)
  {-# INLINE first #-}
  first (Query f) = Query $ \cs -> let !(qS, cs') = f cs in (first qS, cs')

instance ArrowChoice Query where
  {-# INLINE left #-}
  left (Query f) = Query $ \cs -> let !(qS, cs') = f cs in (left qS, cs')

instance ArrowQueryReader Query where
  {-# INLINE fetch #-}
  fetch = fromReader fetch
  {-# INLINE fetchMaybe #-}
  fetchMaybe = fromReader fetchMaybe

instance ArrowDynamicQueryReader Query where
  {-# INLINE entity #-}
  entity = fromReader entity
  {-# INLINE fetchDyn #-}
  fetchDyn = fromReader . fetchDyn
  {-# INLINE fetchMaybeDyn #-}
  fetchMaybeDyn = fromReader . fetchMaybeDyn

instance ArrowDynamicQuery Query where
  {-# INLINE adjustDyn #-}
  adjustDyn f cId = Query (QueryState (ReadsWrites Set.empty (Set.singleton cId)) (adjustDyn f cId),)
  {-# INLINE setDyn #-}
  setDyn cId = Query (QueryState (ReadsWrites Set.empty (Set.singleton cId)) (setDyn cId),)

instance ArrowQuery Query where
  {-# INLINE adjust #-}
  adjust :: forall i a. (Component a) => (i -> a -> a) -> Query i a
  adjust f = Query $ \cs ->
    let !(cId, cs') = CS.insert @a cs
     in (QueryState (ReadsWrites Set.empty (Set.singleton cId)) (adjustDyn f cId), cs')

  {-# INLINE set #-}
  set :: forall a. (Component a) => Query a a
  set = Query $ \cs ->
    let !(cId, cs') = CS.insert @a cs
     in (QueryState (ReadsWrites Set.empty (Set.singleton cId)) (setDyn cId), cs')

{-# INLINE fromReader #-}
fromReader :: QueryReader i o -> Query i o
fromReader (QueryReader f) = Query $ \cs ->
  let !(qS, cs') = f cs
   in (QueryState (ReadsWrites (queryReaderStateReads qS) Set.empty) (fromDynReader $ queryReaderStateDyn qS), cs')

{-# INLINE toReader #-}
toReader :: Query i o -> QueryReader i o
toReader (Query f) = QueryReader $ \cs -> let !(qS, cs') = f cs in (queryStateToReader qS, cs')

-- | Reads and writes of a `Query`.
data ReadsWrites = ReadsWrites
  { reads :: {-# UNPACK #-} !(Set ComponentID),
    writes :: !(Set ComponentID)
  }
  deriving (Show)

instance Semigroup ReadsWrites where
  {-# INLINE (<>) #-}
  ReadsWrites r1 w1 <> ReadsWrites r2 w2 = ReadsWrites (r1 <> r2) (w1 <> w2)

instance Monoid ReadsWrites where
  {-# INLINE mempty #-}
  mempty = ReadsWrites mempty mempty

-- | `True` if the reads and writes of two `Query`s overlap.
disjoint :: ReadsWrites -> ReadsWrites -> Bool
disjoint a b =
  Set.disjoint (reads a) (writes b)
    || Set.disjoint (reads b) (writes a)
    || Set.disjoint (writes b) (writes a)

-- | Match all entities.
all :: i -> Query i a -> Entities -> ([a], Entities)
all i = QR.all i . toReader

-- | Map all matched entities.
map :: i -> Query i a -> Entities -> ([a], Entities)
map i q es =
  let !(qS, cs') = runQuery q (components es)
      !(as, es') = queryStateMap i qS es
   in (as, es' {components = cs'})

-- | State produced by a `Query`.
data QueryState i o = QueryState
  { queryStateReadsWrites :: {-# UNPACK #-} !ReadsWrites,
    queryStateDyn :: !(DynamicQuery i o)
  }
  deriving (Functor)

instance Applicative (QueryState i) where
  {-# INLINE pure #-}
  pure a = QueryState mempty (pure a)
  {-# INLINE (<*>) #-}
  QueryState aRWs aQ <*> QueryState bRWs bQ = QueryState (aRWs <> bRWs) (aQ <*> bQ)

instance Category QueryState where
  {-# INLINE id #-}
  id = QueryState mempty id
  {-# INLINE (.) #-}
  QueryState aRWs aQ . QueryState bRWs bQ = QueryState (aRWs <> bRWs) (aQ . bQ)

instance Arrow QueryState where
  {-# INLINE arr #-}
  arr f = QueryState mempty (arr f)
  {-# INLINE first #-}
  first (QueryState rw q) = QueryState rw (first q)

instance ArrowChoice QueryState where
  {-# INLINE left #-}
  left (QueryState rw q) = QueryState rw (left q)

{-# INLINE queryStateFromReader #-}
queryStateFromReader :: QueryReaderState i o -> QueryState i o
queryStateFromReader (QueryReaderState rws q) =
  QueryState (ReadsWrites rws Set.empty) (fromDynReader q)

{-# INLINE queryStateToReader #-}
queryStateToReader :: QueryState i o -> QueryReaderState i o
queryStateToReader (QueryState rws q) = QueryReaderState (reads rws) $ toDynReader q

-- | Match all entities.
{-# INLINE queryStateAll #-}
queryStateAll :: i -> QueryState i a -> Entities -> [a]
queryStateAll i (QueryState rws dynQ) = allDyn (reads rws <> writes rws) i (toDynReader dynQ)

-- | Map all matched entities.
{-# INLINE queryStateMap #-}
queryStateMap :: i -> QueryState i a -> Entities -> ([a], Entities)
queryStateMap i (QueryState rws dynQ) es =
  let !(as, es') = mapDyn (reads rws <> writes rws) i dynQ es in (as, es')
