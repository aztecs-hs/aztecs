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
  ( QueryFilter (..),
    emptyFilter,
    with,
    without,
    Query (..),
    fetch,
    fetchWithId,
    fetchMaybe,
    map,
    mapM,
    mapAccum,
    mapWith,
    zipMapAccum,
    set,
    QueryState (..),
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import qualified Control.Monad as M
import Data.Aztecs.Component
import Data.Aztecs.Entity (EntityID)
import Data.Aztecs.World.Archetype (Archetype)
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Components (Components)
import qualified Data.Aztecs.World.Components as CS
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (all, lookup, map, mapM)

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

with :: forall a. (Component a) => QueryFilter
with =
  emptyFilter
    { filterWith = \cs ->
        let (cId, cs') = CS.insert @a cs
         in (Set.singleton cId, cs')
    }

without :: forall a. (Component a) => QueryFilter
without =
  emptyFilter
    { filterWithout = \cs ->
        let (cId, cs') = CS.insert @a cs
         in (Set.singleton cId, cs')
    }

data QueryState m i o = QueryState
  { queryStateAll :: [i] -> Archetype -> m ([o], Archetype),
    queryStateLookup :: i -> EntityID -> Archetype -> m (Maybe o)
  }

newtype Query m i o = Query {runQuery :: Components -> (Set ComponentID, Components, QueryState m i o)}

instance (Functor m) => Functor (Query m i) where
  fmap f (Query q) = Query $ \cs ->
    let (cIds, cs', qS) = q cs
     in ( cIds,
          cs',
          qS
            { queryStateAll = \i arch -> fmap (\(a, arch') -> (fmap f a, arch')) $ queryStateAll qS i arch,
              queryStateLookup = \i eId arch -> fmap (fmap f) $ queryStateLookup qS i eId arch
            }
        )

instance (Monad m) => Category (Query m) where
  id = Query $ \cs ->
    ( Set.empty,
      cs,
      QueryState
        { queryStateAll = \as arch -> pure (as, arch),
          queryStateLookup = \a _ _ -> pure (Just a)
        }
    )
  (Query f) . (Query g) = Query $ \cs ->
    let (cIdsG, cs', aQS) = g cs
        (cIdsF, cs'', bQS) = f cs'
     in ( cIdsG <> cIdsF,
          cs'',
          QueryState
            { queryStateAll = \i arch -> do
                (as, arch') <- queryStateAll aQS i arch
                queryStateAll bQS as arch',
              queryStateLookup = \i eId arch -> do
                res <- queryStateLookup aQS i eId arch
                case res of
                  Just a -> queryStateLookup bQS a eId arch
                  Nothing -> pure Nothing
            }
        )

instance (Monad m) => Arrow (Query m) where
  arr f = Query $ \cs ->
    ( Set.empty,
      cs,
      QueryState
        { queryStateAll = \bs arch -> pure (fmap f bs, arch),
          queryStateLookup = \b _ _ -> pure $ Just (f b)
        }
    )
  first (Query f) = Query $ \comps ->
    let (cIds, comps', qS) = f comps
     in ( cIds,
          comps',
          QueryState
            { queryStateAll = \bds arch -> do
                let (bs, ds) = unzip bds
                (cs, arch') <- queryStateAll qS bs arch
                return (zip cs ds, arch'),
              queryStateLookup = \(b, d) eId arch -> do
                res <- queryStateLookup qS b eId arch
                return $ case res of
                  Just c -> Just (c, d)
                  Nothing -> Nothing
            }
        )

-- | Fetch a `Component` by its type.
fetch :: forall m a. (Applicative m, Component a) => Query m () a
fetch = fmap snd fetchWithId

-- | Fetch an `EntityID` and `Component` by its type.
fetchWithId :: forall m a. (Applicative m, Component a) => Query m () (EntityID, a)
fetchWithId = Query $ \cs ->
  let (cId, cs') = CS.insert @a cs
   in ( Set.singleton cId,
        cs',
        QueryState
          { queryStateAll = \_ arch -> let as = A.all cId arch in pure (as, arch),
            queryStateLookup = \_ eId arch -> pure $ fmap (\c -> (eId, c)) $ A.lookupComponent eId cId arch
          }
      )

map :: forall m a. (Applicative m, Component a) => (a -> a) -> Query m () (EntityID, a)
map f = Query $ \cs ->
  let (cId, cs') = CS.insert @a cs
   in ( Set.singleton cId,
        cs',
        QueryState
          { queryStateAll = \_ arch ->
              let as = A.all cId arch
                  as' = fmap (\(eId, a) -> (eId, f a)) as
               in pure (as', A.insertAscList cId as' arch),
            queryStateLookup = \_ eId arch -> pure $ fmap (\c -> (eId, f c)) $ A.lookupComponent eId cId arch
          }
      )

mapM :: forall m a. (Monad m, Component a) => (a -> m a) -> Query m () a
mapM f = Query $ \cs ->
  let (cId, cs') = CS.insert @a cs
   in ( Set.singleton cId,
        cs',
        QueryState
          { queryStateAll = \_ arch -> do
              let as = A.all cId arch
              as' <- M.mapM (\(eId, a) -> fmap (\a' -> (eId, a')) (f a)) as
              return (fmap snd as', A.insertAscList cId as' arch),
            queryStateLookup = \_ eId arch -> case A.lookupComponent eId cId arch of
              Just a -> Just <$> f a
              Nothing -> pure Nothing
          }
      )

mapAccum ::
  forall m b a.
  (Monad m, Component a) =>
  (a -> m (b, a)) ->
  Query m () (b, a)
mapAccum f = Query $ \cs ->
  let (cId, cs') = CS.insert @a cs
   in ( Set.singleton cId,
        cs',
        QueryState
          { queryStateAll = \_ arch -> do
              let as = A.all @a cId arch
              bas <-
                M.mapM
                  ( \(eId, a) -> do
                      (b, a') <- f a
                      return ((b, a'), (eId, a'))
                  )
                  as
              let (bs, as') = unzip bas
              return (bs, A.insertAscList cId as' arch),
            queryStateLookup = \_ eId arch -> case A.lookupComponent eId cId arch of
              Just a -> do
                (b, a') <- f a
                pure $ Just (b, a')
              Nothing -> pure Nothing
          }
      )

zipMapAccum ::
  forall m i b a.
  (Monad m, Component a) =>
  (i -> a -> m (b, a)) ->
  Query m i (EntityID, b, a)
zipMapAccum f = Query $ \cs ->
  let (cId, cs') = CS.insert @a cs
   in ( Set.singleton cId,
        cs',
        QueryState
          { queryStateAll = \i arch -> do
              let as = A.all @a cId arch
              bs_as' <-
                M.mapM
                  ( \((eId, a), i') -> do
                      (b, a') <- f i' a
                      return ((eId, b, a'), (eId, a'))
                  )
                  (zip as i)
              let (bs, as') = unzip bs_as'
              return (bs, A.insertAscList cId as' arch),
            queryStateLookup = \i eId arch -> case A.lookupComponent eId cId arch of
              Just a -> do
                (b, a') <- f i a
                pure $ Just (eId, b, a')
              Nothing -> pure Nothing
          }
      )

mapWith ::
  forall m i a.
  (Applicative m, Component a) =>
  (i -> a -> a) ->
  Query m i (EntityID, a)
mapWith f = Query $ \cs ->
  let (cId, cs') = CS.insert @a cs
   in ( Set.singleton cId,
        cs',
        QueryState
          { queryStateAll = \is arch ->
              let as = A.all cId arch
                  as' = fmap (\((eId, a), i) -> (eId, f i a)) (zip as is)
               in pure (as', A.insertAscList cId as' arch),
            queryStateLookup = \i eId arch -> pure $ fmap (\c -> (eId, f i c)) $ A.lookupComponent eId cId arch
          }
      )

set ::
  forall m a.
  (Applicative m, Component a) =>
  Query m a a
set = Query $ \cs ->
  let (cId, cs') = CS.insert @a cs
   in ( Set.singleton cId,
        cs',
        QueryState
          { queryStateAll = \is arch -> pure (is, A.withAscList cId is arch),
            queryStateLookup = \i eId arch -> pure $ A.lookupComponent eId cId arch
          }
      )

fetchMaybe :: forall m a. (Applicative m, Component a) => Query m () (EntityID, Maybe a)
fetchMaybe = fetchMaybe' (CS.insert @a)

fetchMaybe' ::
  forall m a.
  (Applicative m, Component a) =>
  (Components -> (ComponentID, Components)) ->
  Query m () ((EntityID, Maybe a))
fetchMaybe' f = Query $ \cs ->
  let (cId, cs') = f cs
   in ( Set.singleton cId,
        cs',
        QueryState
          { queryStateAll = \_ arch -> let as = A.allMaybe cId arch in pure (as, arch),
            queryStateLookup = \_ eId arch -> pure $ fmap (\c -> (eId, Just c)) $ A.lookupComponent eId cId arch
          }
      )
