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
import Aztecs.ECS.Query.Dynamic (DynamicQuery (..), fromDynReader, mapDyn, toDynReader)
import Aztecs.ECS.Query.Dynamic.Class (ArrowDynamicQuery (..))
import Aztecs.ECS.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..))
import Aztecs.ECS.Query.Reader (QueryFilter (..), QueryReader (..), with, without)
import qualified Aztecs.ECS.Query.Reader as QR
import Aztecs.ECS.Query.Reader.Class (ArrowQueryReader (..))
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Arrow (Arrow (..), ArrowChoice (..))
import Control.Category (Category (..))
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
newtype Query i o = Query {runQuery :: Components -> (ReadsWrites, Components, DynamicQuery i o)}
  deriving (Functor)

instance Applicative (Query i) where
  {-# INLINE pure #-}
  pure a = Query (mempty,,pure a)
  {-# INLINE (<*>) #-}
  (Query f) <*> (Query g) = Query $ \cs ->
    let !(cIdsG, cs', aQS) = g cs
        !(cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS <*> aQS)

instance Category Query where
  {-# INLINE id #-}
  id = Query (mempty,,id)
  {-# INLINE (.) #-}
  (Query f) . (Query g) = Query $ \cs ->
    let !(cIdsG, cs', aQS) = g cs
        !(cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS . aQS)

instance Arrow Query where
  {-# INLINE arr #-}
  arr f = Query (mempty,,arr f)
  {-# INLINE first #-}
  first (Query f) = Query $ \comps -> let !(cIds, comps', qS) = f comps in (cIds, comps', first qS)

instance ArrowChoice Query where
  {-# INLINE left #-}
  left (Query f) = Query $ \comps -> let !(cIds, comps', qS) = f comps in (cIds, comps', left qS)

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
  adjustDyn f cId = Query (ReadsWrites Set.empty (Set.singleton cId),,adjustDyn f cId)
  {-# INLINE setDyn #-}
  setDyn cId = Query (ReadsWrites Set.empty (Set.singleton cId),,setDyn cId)

instance ArrowQuery Query where
  {-# INLINE adjust #-}
  adjust :: forall i a. (Component a) => (i -> a -> a) -> Query i a
  adjust f = Query $ \cs ->
    let !(cId, cs') = CS.insert @a cs
     in (ReadsWrites Set.empty (Set.singleton cId), cs', adjustDyn f cId)
  {-# INLINE set #-}
  set :: forall a. (Component a) => Query a a
  set = Query $ \cs ->
    let !(cId, cs') = CS.insert @a cs
     in (ReadsWrites Set.empty (Set.singleton cId), cs', setDyn cId)

{-# INLINE fromReader #-}
fromReader :: QueryReader i o -> Query i o
fromReader (QueryReader f) = Query $ \cs ->
  let !(cIds, cs', dynQ) = f cs in (ReadsWrites cIds Set.empty, cs', fromDynReader dynQ)

{-# INLINE toReader #-}
toReader :: Query i o -> QueryReader i o
toReader (Query f) = QueryReader $ \cs ->
  let !(rws, cs', dynQ) = f cs in (reads rws, cs', toDynReader dynQ)

-- | Reads and writes of a `Query`.
data ReadsWrites = ReadsWrites
  { reads :: {-# UNPACK #-} !(Set ComponentID),
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

-- | Map all matched entities.
{-# INLINE map #-}
map :: i -> Query i a -> Entities -> ([a], Entities)
map i q es =
  let !(rws, cs', dynQ) = runQuery q $ components es
      !cIds = reads rws <> writes rws
      !(as, es') = mapDyn cIds i dynQ es
   in (as, es' {components = cs'})
