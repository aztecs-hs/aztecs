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
    fetch,
    fetchWithId,
    fetchMaybe,
    set,
    run,

    -- * Filters
    QueryFilter (..),
    emptyFilter,
    with,
    without,

    -- * Dynamic queries
    DynamicQuery (..),
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Monad (mapM)
import Data.Aztecs.Component
import Data.Aztecs.Entity (EntityID)
import Data.Aztecs.World.Archetype (Archetype)
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Components (Components)
import qualified Data.Aztecs.World.Components as CS
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (all, id, lookup, map, mapM, (.))

newtype Query m i o = Query {runQuery :: Components -> (Set ComponentID, Components, DynamicQuery m i o)}

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

-- | Fetch a `Component` by its type.
fetch :: forall m a. (Applicative m, Component a) => Query m () a
fetch = fmap snd fetchWithId

-- | Fetch an `EntityID` and `Component` by its type.
fetchWithId :: forall m a. (Applicative m, Component a) => Query m () (EntityID, a)
fetchWithId = Query $ \cs ->
  let (cId, cs') = CS.insert @a cs in (Set.singleton cId, cs', fetchDynWithId cId)

fetchMaybe :: forall m a. (Applicative m, Component a) => Query m () (EntityID, Maybe a)
fetchMaybe = Query $ \cs ->
  let (cId, cs') = CS.insert @a cs in (Set.singleton cId, cs', fetchMaybeDynWithId cId)

set ::
  forall m a.
  (Applicative m, Component a) =>
  Query m a a
set = Query $ \cs ->
  let (cId, cs') = CS.insert @a cs
   in ( Set.singleton cId,
        cs',
        DynamicQuery
          { dynQueryAll = \is arch -> pure (is, A.withAscList cId is arch),
            dynQueryLookup = \i eId arch -> pure $ A.lookupComponent eId cId arch
          }
      )

run :: (Monad m) => (i -> m o) -> Query m i o
run f = Query $ \cs ->
  ( Set.empty,
    cs,
    DynamicQuery
      { dynQueryAll = \is arch -> (,arch) <$> mapM f is,
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
  { dynQueryAll :: [i] -> Archetype -> m ([o], Archetype),
    dynQueryLookup :: i -> EntityID -> Archetype -> m (Maybe o)
  }

instance (Functor m) => Functor (DynamicQuery m i) where
  fmap f q =
    DynamicQuery
      { dynQueryAll = \i arch -> fmap (\(a, arch') -> (fmap f a, arch')) $ dynQueryAll q i arch,
        dynQueryLookup = \i eId arch -> fmap (fmap f) $ dynQueryLookup q i eId arch
      }

instance (Monad m) => Category (DynamicQuery m) where
  id =
    DynamicQuery
      { dynQueryAll = \as arch -> pure (as, arch),
        dynQueryLookup = \a _ _ -> pure (Just a)
      }

  f . g =
    DynamicQuery
      { dynQueryAll = \i arch -> do
          (as, arch') <- dynQueryAll g i arch
          dynQueryAll f as arch',
        dynQueryLookup = \i eId arch -> do
          res <- dynQueryLookup g i eId arch
          case res of
            Just a -> dynQueryLookup f a eId arch
            Nothing -> pure Nothing
      }

instance (Monad m) => Arrow (DynamicQuery m) where
  arr f =
    DynamicQuery
      { dynQueryAll = \bs arch -> pure (fmap f bs, arch),
        dynQueryLookup = \b _ _ -> pure $ Just (f b)
      }
  first f =
    DynamicQuery
      { dynQueryAll = \bds arch -> do
          let (bs, ds) = unzip bds
          (cs, arch') <- dynQueryAll f bs arch
          return (zip cs ds, arch'),
        dynQueryLookup = \(b, d) eId arch -> do
          res <- dynQueryLookup f b eId arch
          return $ case res of
            Just c -> Just (c, d)
            Nothing -> Nothing
      }

-- | Fetch an `EntityID` and `Component` by its `ComponentID`.
fetchDynWithId ::
  forall m a.
  (Applicative m, Component a) =>
  ComponentID ->
  DynamicQuery m () (EntityID, a)
fetchDynWithId cId =
  DynamicQuery
    { dynQueryAll = \_ arch -> let as = A.all cId arch in pure (as, arch),
      dynQueryLookup = \_ eId arch -> pure $ fmap (\c -> (eId, c)) $ A.lookupComponent eId cId arch
    }

-- | Fetch an `EntityID` and `Component` by its `ComponentID`.
fetchMaybeDynWithId ::
  forall m a.
  (Applicative m, Component a) =>
  ComponentID ->
  DynamicQuery m () ((EntityID, Maybe a))
fetchMaybeDynWithId cId =
  DynamicQuery
    { dynQueryAll = \_ arch -> let as = A.allMaybe cId arch in pure (as, arch),
      dynQueryLookup = \_ eId arch -> pure $ fmap (\c -> (eId, Just c)) $ A.lookupComponent eId cId arch
    }
