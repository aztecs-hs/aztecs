{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.Query
  ( -- * Queries
    Query (..),
    entity,
    fetch,
    fetchMaybe,
    set,
    run,
    all,

    -- * Filters
    QueryFilter (..),
    with,
    without,
    DynamicQueryFilter (..),

    -- * Dynamic queries
    DynamicQuery (..),
    entityDyn,
    fetchDyn,
    fetchMaybeDyn,
    setDyn,

    -- * Reads and writes
    ReadsWrites (..),
    disjoint,
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Monad (mapM)
import Data.Aztecs.Component
import Data.Aztecs.Entity (EntityID)
import Data.Aztecs.World (World (..))
import Data.Aztecs.World.Archetype (Archetype)
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

-- | Get the currently matched `EntityID`.
entity :: (Applicative m) => Query m () EntityID
entity = Query $ \cs -> (mempty, cs, entityDyn)

-- | Fetch a `Component` by its type.
fetch :: forall m a. (Applicative m, Component a) => Query m () (a)
fetch = Query $ \cs ->
  let (cId, cs') = CS.insert @a cs
   in (ReadsWrites (Set.singleton cId) (Set.empty), cs', fetchDyn cId)

-- | Fetch a `Component` by its type, returning `Nothing` if it doesn't exist.
fetchMaybe :: forall m a. (Applicative m, Component a) => Query m () (Maybe a)
fetchMaybe = Query $ \cs ->
  let (cId, cs') = CS.insert @a cs
   in (ReadsWrites (Set.singleton cId) (Set.empty), cs', fetchMaybeDyn cId)

-- | Set a `Component` by its type.
set :: forall m a. (Applicative m, Component a) => Query m a a
set = Query $ \cs ->
  let (cId, cs') = CS.insert @a cs
   in (ReadsWrites Set.empty (Set.singleton cId), cs', setDyn cId)

-- | Run a monadic task in a `Query`.
run :: (Monad m) => (i -> m o) -> Query m i o
run f = Query $ \cs ->
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

data DynamicQueryFilter = DynamicQueryFilter
  { filterWith :: !(Set ComponentID),
    filterWithout :: !(Set ComponentID)
  }

instance Semigroup DynamicQueryFilter where
  DynamicQueryFilter withA withoutA <> DynamicQueryFilter withB withoutB =
    DynamicQueryFilter (withA <> withB) (withoutA <> withoutB)

instance Monoid DynamicQueryFilter where
  mempty = DynamicQueryFilter mempty mempty

-- | Dynamic query for components by ID.
data DynamicQuery m i o = DynamicQuery
  { dynQueryAll :: !([i] -> [EntityID] -> Archetype -> m ([o], Archetype)),
    dynQueryLookup :: !(i -> EntityID -> Archetype -> m (Maybe o, Archetype))
  }

instance (Functor m) => Functor (DynamicQuery m i) where
  fmap f q =
    DynamicQuery
      { dynQueryAll =
          \i es arch -> fmap (\(a, arch') -> (fmap f a, arch')) $ dynQueryAll q i es arch,
        dynQueryLookup = \i eId arch -> fmap (first $ fmap f) $ dynQueryLookup q i eId arch
      }

instance (Monad m) => Applicative (DynamicQuery m i) where
  pure a =
    DynamicQuery
      { dynQueryAll = \_ es arch -> pure (take (length es) $ repeat a, arch),
        dynQueryLookup = \_ _ arch -> pure (Just a, arch)
      }
  f <*> g =
    DynamicQuery
      { dynQueryAll = \i es arch -> do
          (as, arch') <- dynQueryAll g i es arch
          (fs, arch'') <- dynQueryAll f i es arch'
          return (zipWith ($) fs as, arch''),
        dynQueryLookup = \i eId arch -> do
          (res, arch') <- dynQueryLookup g i eId arch
          case res of
            Just a -> do
              (res', arch'') <- dynQueryLookup f i eId arch'
              return (fmap ($) res' <*> Just a, arch'')
            Nothing -> pure (Nothing, arch')
      }

instance (Monad m) => Category (DynamicQuery m) where
  id =
    DynamicQuery
      { dynQueryAll = \as _ arch -> pure (as, arch),
        dynQueryLookup = \a _ arch -> pure (Just a, arch)
      }
  f . g =
    DynamicQuery
      { dynQueryAll = \i es arch -> do
          (as, arch') <- dynQueryAll g i es arch
          dynQueryAll f as es arch',
        dynQueryLookup = \i eId arch -> do
          (res, arch') <- dynQueryLookup g i eId arch
          case res of
            Just a -> dynQueryLookup f a eId arch'
            Nothing -> pure (Nothing, arch')
      }

instance (Monad m) => Arrow (DynamicQuery m) where
  arr f =
    DynamicQuery
      { dynQueryAll = \bs _ arch -> pure (fmap f bs, arch),
        dynQueryLookup = \b _ arch -> pure (Just (f b), arch)
      }
  first f =
    DynamicQuery
      { dynQueryAll = \bds es arch -> do
          let (bs, ds) = unzip bds
          (cs, arch') <- dynQueryAll f bs es arch
          return (zip cs ds, arch'),
        dynQueryLookup = \(b, d) eId arch -> do
          (res, arch') <- dynQueryLookup f b eId arch
          return
            ( case res of
                Just c -> Just (c, d)
                Nothing -> Nothing,
              arch'
            )
      }

-- | Fetch the `EntityID` belonging to this entity.
entityDyn :: (Applicative m) => DynamicQuery m i EntityID
entityDyn =
  DynamicQuery
    { dynQueryAll = \_ es arch -> pure (es, arch),
      dynQueryLookup = \_ eId arch -> pure $ (Just eId, arch)
    }

-- | Fetch an `Component` by its `ComponentID`.
fetchDyn :: forall m a. (Applicative m, Component a) => ComponentID -> DynamicQuery m () a
fetchDyn cId =
  DynamicQuery
    { dynQueryAll = \_ _ arch -> let !as = A.all cId arch in pure (fmap snd as, arch),
      dynQueryLookup = \_ eId arch -> pure $ (A.lookupComponent eId cId arch, arch)
    }

-- | Fetch an `EntityID` and `Component` by its `ComponentID`.
fetchMaybeDyn ::
  forall m a.
  (Applicative m, Component a) =>
  ComponentID ->
  DynamicQuery m () (Maybe a)
fetchMaybeDyn cId =
  DynamicQuery
    { dynQueryAll = \_ _ arch -> let as = A.allMaybe cId arch in pure (fmap snd as, arch),
      dynQueryLookup = \_ eId arch -> pure $ (Just <$> A.lookupComponent eId cId arch, arch)
    }

-- | Set a `Component` by its `ComponentID`.
setDyn ::
  forall m a.
  (Applicative m, Component a) =>
  ComponentID ->
  DynamicQuery m a a
setDyn cId =
  DynamicQuery
    { dynQueryAll = \is _ arch -> let !arch' = A.withAscList cId is arch in pure (is, arch'),
      dynQueryLookup =
        \i eId arch -> pure (A.lookupComponent eId cId arch, A.insertComponent eId cId i arch)
    }
