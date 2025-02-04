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
    set,
    run,
    QueryState (..),
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

run :: (Monad m) => (i -> m o) -> Query m i o
run f = Query $ \cs ->
  ( Set.empty,
    cs,
    QueryState
      { queryStateAll = \is arch -> (,arch) <$> mapM f is,
        queryStateLookup = \i _ _ -> Just <$> f i
      }
  )
