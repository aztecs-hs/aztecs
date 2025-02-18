{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.Query
  ( -- * Queries
    Query (..),
    task,
    ArrowQuery (..),
    ArrowQueryReader (..),

    -- ** Running queries
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

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Monad (mapM)
import Data.Aztecs.Component
import Data.Aztecs.Query.Class (ArrowQuery (..))
import Data.Aztecs.Query.Dynamic (DynamicQuery (..))
import Data.Aztecs.Query.Dynamic.Class (ArrowDynamicQuery (..))
import Data.Aztecs.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..))
import Data.Aztecs.Query.Reader (QueryFilter (..), with, without)
import Data.Aztecs.Query.Reader.Class (ArrowQueryReader (..))
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Archetypes (Node (nodeArchetype))
import qualified Data.Aztecs.World.Archetypes as AS
import Data.Aztecs.World.Components (Components)
import qualified Data.Aztecs.World.Components as CS
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (all, any, id, lookup, map, mapM, reads, (.))

-- | Query for matching entities.
--
-- === Do notation:
-- > move :: (Monad m) => Query m () Position
-- > move = proc () -> do
-- >   Velocity v <- Q.fetch -< ()
-- >   Position p <- Q.fetch -< ()
-- >   Q.set -< Position $ p + v
--
-- === Arrow combinators:
-- > move :: (Monad m) => Query m () Position
-- > move = Q.fetch &&& Q.fetch >>> arr (\(Position p, Velocity v) -> Position $ p + v) >>> Q.set
--
-- === Applicative combinators:
-- > move :: (Monad m) => Query m () Position
-- > move = (,) <$> Q.fetch <*> Q.fetch >>> arr (\(Position p, Velocity v) -> Position $ p + v) >>> Q.set
newtype Query m i o
  = Query {runQuery :: Components -> (ReadsWrites, Components, DynamicQuery m i o)}

instance (Functor m) => Functor (Query m i) where
  fmap f (Query q) = Query $ \cs -> let (cIds, cs', qS) = q cs in (cIds, cs', fmap f qS)

instance (Monad m) => Applicative (Query m i) where
  pure a = Query $ \cs -> (mempty, cs, pure a)
  (Query f) <*> (Query g) = Query $ \cs ->
    let (cIdsG, cs', aQS) = g cs
        (cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS <*> aQS)

instance (Monad m) => Category (Query m) where
  id = Query $ \cs -> (mempty, cs, id)
  (Query f) . (Query g) = Query $ \cs ->
    let (cIdsG, cs', aQS) = g cs
        (cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS . aQS)

instance (Monad m) => Arrow (Query m) where
  arr f = Query $ \cs -> (mempty, cs, arr f)
  first (Query f) = Query $ \comps -> let (cIds, comps', qS) = f comps in (cIds, comps', first qS)

instance (Monad m) => ArrowQueryReader (Query m) where
  entity = Query $ \cs -> (mempty, cs, entityDyn)
  fetch :: forall a. (Component a) => Query m () a
  fetch = Query $ \cs ->
    let (cId, cs') = CS.insert @a cs
     in (ReadsWrites (Set.singleton cId) (Set.empty), cs', fetchDyn cId)
  fetchMaybe :: forall a. (Component a) => Query m () (Maybe a)
  fetchMaybe = Query $ \cs ->
    let (cId, cs') = CS.insert @a cs
     in (ReadsWrites (Set.singleton cId) (Set.empty), cs', fetchMaybeDyn cId)

instance (Monad m) => ArrowQuery (Query m) where
  set :: forall a. (Applicative m, Component a) => Query m a a
  set = Query $ \cs ->
    let (cId, cs') = CS.insert @a cs
     in (ReadsWrites Set.empty (Set.singleton cId), cs', setDyn cId)

-- | Run a monadic task in a `Query`.
task :: (Monad m) => (i -> m o) -> Query m i o
task f = Query $ \cs ->
  ( mempty,
    cs,
    DynamicQuery
      { dynQueryAll = \is _ arch -> (,arch) <$> mapM f is,
        dynQueryLookup = \i _ arch -> (\a -> (Just a, arch)) <$> f i
      }
  )

-- | Query all matching entities.
--
-- >>> :set -XTypeApplications
-- >>> import Data.Aztecs
-- >>> import qualified Data.Aztecs.World as W
-- >>>
-- >>> newtype X = X Int deriving (Show)
-- >>> instance Component X
-- >>>
-- >>> let (_, w) = W.spawn (bundle $ X 0) W.empty
-- >>> (xs, _) <- all (fetch @_ @X) w
-- >>> xs
-- [X 0]
all :: (Monad m) => Query m () a -> World -> m ([a], World)
all q w = do
  let (rws, cs', dynQ) = runQuery q (components w)
  as <-
    mapM
      (\n -> fst <$> dynQueryAll dynQ (repeat ()) (A.entities $ nodeArchetype n) (nodeArchetype n))
      (Map.elems $ AS.lookup (reads rws <> writes rws) (archetypes w))
  return (concat as, w {components = cs'})

data ReadsWrites = ReadsWrites
  { reads :: !(Set ComponentID),
    writes :: !(Set ComponentID)
  }
  deriving (Show)

instance Semigroup ReadsWrites where
  ReadsWrites r1 w1 <> ReadsWrites r2 w2 = ReadsWrites (r1 <> r2) (w1 <> w2)

instance Monoid ReadsWrites where
  mempty = ReadsWrites mempty mempty

disjoint :: ReadsWrites -> ReadsWrites -> Bool
disjoint a b =
  Set.disjoint (reads a) (writes b)
    || Set.disjoint (reads b) (writes a)
    || Set.disjoint (writes b) (writes a)
