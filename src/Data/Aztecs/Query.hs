{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
    emptyFilter,
    with,
    without,

    -- * Dynamic queries
    DynamicQuery (..),
    entityDyn,
    fetchDyn,
    fetchMaybeDyn,
    setDyn,
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
import qualified Data.Aztecs.World.Archetypes as AS
import Data.Aztecs.World.Components (Components)
import qualified Data.Aztecs.World.Components as CS
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (all, id, lookup, map, mapM, (.))

newtype Query m i o
  = Query {runQuery :: Components -> (Set ComponentID, Components, DynamicQuery m i o)}

instance (Functor m) => Functor (Query m i) where
  fmap f (Query q) = Query $ \cs -> let (cIds, cs', qS) = q cs in (cIds, cs', fmap f qS)

instance (Monad m) => Category (Query m) where
  id = Query $ \cs -> (Set.empty, cs, id)
  (Query f) . (Query g) = Query $ \cs ->
    let (cIdsG, cs', aQS) = g cs
        (cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS . aQS)

instance (Monad m) => Arrow (Query m) where
  arr f = Query $ \cs -> (Set.empty, cs, arr f)
  first (Query f) = Query $ \comps -> let (cIds, comps', qS) = f comps in (cIds, comps', first qS)

entity :: (Applicative m) => Query m () EntityID
entity = Query $ \cs -> (Set.empty, cs, entityDyn)

-- | Fetch a`Component` by its type.
fetch :: forall m a. (Applicative m, Component a) => Query m () (a)
fetch = Query $ \cs ->
  let (cId, cs') = CS.insert @a cs in (Set.singleton cId, cs', fetchDyn cId)

fetchMaybe :: forall m a. (Applicative m, Component a) => Query m () (Maybe a)
fetchMaybe = Query $ \cs ->
  let (cId, cs') = CS.insert @a cs in (Set.singleton cId, cs', fetchMaybeDyn cId)

set ::
  forall m a.
  (Applicative m, Component a) =>
  Query m a a
set = Query $ \cs -> let (cId, cs') = CS.insert @a cs in (Set.singleton cId, cs', setDyn cId)

run :: (Monad m) => (i -> m o) -> Query m i o
run f = Query $ \cs ->
  ( Set.empty,
    cs,
    DynamicQuery
      { dynQueryAll = \is _ arch -> (,arch) <$> mapM f is,
        dynQueryLookup = \i _ _ -> Just <$> f i
      }
  )

-- | Filter for a `Query`.
data QueryFilter = QueryFilter
  { filterWith :: Components -> (Set ComponentID, Components),
    filterWithout :: Components -> (Set ComponentID, Components)
  }

instance Semigroup QueryFilter where
  QueryFilter withA withoutA <> QueryFilter withB withoutB =
    QueryFilter
      ( \cs ->
          let (withA', cs') = withA cs
              (withB', cs'') = withB cs'
           in (withA' <> withB', cs'')
      )
      ( \cs ->
          let (withoutA', cs') = withoutA cs
              (withoutB', cs'') = withoutB cs'
           in (withoutA' <> withoutB', cs'')
      )

instance Monoid QueryFilter where
  mempty = emptyFilter

emptyFilter :: QueryFilter
emptyFilter = QueryFilter (Set.empty,) (Set.empty,)

-- | Filter for entities containing this component.
with :: forall a. (Component a) => QueryFilter
with =
  emptyFilter
    { filterWith = \cs ->
        let (cId, cs') = CS.insert @a cs
         in (Set.singleton cId, cs')
    }

-- | Filter out entities containing this component.
without :: forall a. (Component a) => QueryFilter
without =
  emptyFilter
    { filterWithout = \cs ->
        let (cId, cs') = CS.insert @a cs
         in (Set.singleton cId, cs')
    }

-- | Dynamic query for components by ID.
data DynamicQuery m i o = DynamicQuery
  { dynQueryAll :: [i] -> [EntityID] -> Archetype -> m ([o], Archetype),
    dynQueryLookup :: i -> EntityID -> Archetype -> m (Maybe o)
  }

instance (Functor m) => Functor (DynamicQuery m i) where
  fmap f q =
    DynamicQuery
      { dynQueryAll =
          \i es arch -> fmap (\(a, arch') -> (fmap f a, arch')) $ dynQueryAll q i es arch,
        dynQueryLookup = \i eId arch -> fmap (fmap f) $ dynQueryLookup q i eId arch
      }

instance (Monad m) => Category (DynamicQuery m) where
  id =
    DynamicQuery
      { dynQueryAll = \as _ arch -> pure (as, arch),
        dynQueryLookup = \a _ _ -> pure (Just a)
      }

  f . g =
    DynamicQuery
      { dynQueryAll = \i es arch -> do
          (as, arch') <- dynQueryAll g i es arch
          dynQueryAll f as es arch',
        dynQueryLookup = \i eId arch -> do
          res <- dynQueryLookup g i eId arch
          case res of
            Just a -> dynQueryLookup f a eId arch
            Nothing -> pure Nothing
      }

instance (Monad m) => Arrow (DynamicQuery m) where
  arr f =
    DynamicQuery
      { dynQueryAll = \bs _ arch -> pure (fmap f bs, arch),
        dynQueryLookup = \b _ _ -> pure $ Just (f b)
      }
  first f =
    DynamicQuery
      { dynQueryAll = \bds es arch -> do
          let (bs, ds) = unzip bds
          (cs, arch') <- dynQueryAll f bs es arch
          return (zip cs ds, arch'),
        dynQueryLookup = \(b, d) eId arch -> do
          res <- dynQueryLookup f b eId arch
          return $ case res of
            Just c -> Just (c, d)
            Nothing -> Nothing
      }

-- | Fetch the `EntityID` belonging to this entity.
entityDyn :: (Applicative m) => DynamicQuery m i EntityID
entityDyn =
  DynamicQuery
    { dynQueryAll = \_ es arch -> pure (es, arch),
      dynQueryLookup = \_ eId _ -> pure $ Just eId
    }

-- | Fetch an `Component` by its `ComponentID`.
fetchDyn ::
  forall m a.
  (Applicative m, Component a) =>
  ComponentID ->
  DynamicQuery m () a
fetchDyn cId =
  DynamicQuery
    { dynQueryAll = \_ _ arch -> let as = A.all cId arch in pure (fmap snd as, arch),
      dynQueryLookup = \_ eId arch -> pure $ A.lookupComponent eId cId arch
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
      dynQueryLookup = \_ eId arch -> pure $ Just <$> A.lookupComponent eId cId arch
    }

setDyn ::
  forall m a.
  (Applicative m, Component a) =>
  ComponentID ->
  DynamicQuery m a a
setDyn cId =
  DynamicQuery
    { dynQueryAll = \is _ arch -> pure (is, A.withAscList cId is arch),
      dynQueryLookup = \i eId arch -> pure $ A.lookupComponent eId cId arch
    }

all :: (Monad m) => Query m () a -> World -> m ([a], World)
all q w = do
  let (cIds, cs', dynQ) = runQuery q (components w)
  as <-
    mapM
      (\arch -> fst <$> dynQueryAll dynQ (repeat ()) (A.entities arch) arch)
      (Map.elems $ AS.lookup cIds (archetypes w))
  return (concat as, w {components = cs'})
