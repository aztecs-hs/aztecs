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
import Aztecs.ECS.Query.Dynamic (DynamicQuery (..), fromDynReader)
import Aztecs.ECS.Query.Dynamic.Class (ArrowDynamicQuery (..))
import Aztecs.ECS.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..))
import Aztecs.ECS.Query.Reader (QueryFilter (..), QueryReader (..), with, without)
import Aztecs.ECS.Query.Reader.Class (ArrowQueryReader (..))
import Aztecs.ECS.World (World (..))
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Control.Arrow (Arrow (..), ArrowChoice (..))
import Control.Category (Category (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (all, id, reads, (.))

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
  pure a = Query (mempty,,pure a)
  (Query f) <*> (Query g) = Query $ \cs ->
    let (cIdsG, cs', aQS) = g cs
        (cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS <*> aQS)

instance Category Query where
  id = Query (mempty,,id)
  (Query f) . (Query g) = Query $ \cs ->
    let (cIdsG, cs', aQS) = g cs
        (cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS . aQS)

instance Arrow Query where
  arr f = Query (mempty,,arr f)
  first (Query f) = Query $ \comps -> let (cIds, comps', qS) = f comps in (cIds, comps', first qS)

instance ArrowChoice Query where
  left (Query f) = Query $ \comps -> let (cIds, comps', qS) = f comps in (cIds, comps', left qS)

instance ArrowQueryReader Query where
  fetch = fromReader fetch
  fetchMaybe = fromReader fetchMaybe

instance ArrowDynamicQueryReader Query where
  entity = fromReader entity
  fetchDyn = fromReader . fetchDyn
  fetchMaybeDyn = fromReader . fetchMaybeDyn

instance ArrowDynamicQuery Query where
  setDyn cId = Query (ReadsWrites Set.empty (Set.singleton cId),,setDyn cId)

instance ArrowQuery Query where
  set :: forall a. (Component a) => Query a a
  set = Query $ \cs ->
    let (cId, cs') = CS.insert @a cs
     in (ReadsWrites Set.empty (Set.singleton cId), cs', setDyn cId)

fromReader :: QueryReader i o -> Query i o
fromReader (QueryReader f) = Query $ \cs ->
  let (cIds, cs', dynQ) = f cs in (ReadsWrites cIds Set.empty, cs', fromDynReader dynQ)

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
all :: Query () a -> World -> ([a], World)
all q w =
  let (rws, cs', dynQ) = runQuery q (components w)
      as =
        fmap
          (\n -> fst $ dynQueryAll dynQ (repeat ()) (A.entities $ nodeArchetype n) (nodeArchetype n))
          (AS.find (reads rws <> writes rws) (archetypes w))
   in (concat as, w {components = cs'})
