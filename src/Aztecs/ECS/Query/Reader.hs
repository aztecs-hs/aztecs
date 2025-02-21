{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.Query.Reader
  ( -- * Queries
    QueryReader (..),
    ArrowQueryReader (..),
    ArrowDynamicQueryReader (..),

    -- * Filters
    QueryFilter (..),
    with,
    without,
    DynamicQueryFilter (..),
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic (DynamicQueryFilter (..))
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReader (..))
import Aztecs.ECS.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..))
import Aztecs.ECS.Query.Reader.Class (ArrowQueryReader (..))
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (id, (.))

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
newtype QueryReader i o
  = Query {runQueryReader :: Components -> (Set ComponentID, Components, DynamicQueryReader i o)}

instance Functor (QueryReader i) where
  fmap f (Query q) = Query $ \cs -> let (cIds, cs', qS) = q cs in (cIds, cs', fmap f qS)

instance Applicative (QueryReader i) where
  pure a = Query $ \cs -> (mempty, cs, pure a)
  (Query f) <*> (Query g) = Query $ \cs ->
    let (cIdsG, cs', aQS) = g cs
        (cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS <*> aQS)

instance Category QueryReader where
  id = Query $ \cs -> (mempty, cs, id)
  (Query f) . (Query g) = Query $ \cs ->
    let (cIdsG, cs', aQS) = g cs
        (cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS . aQS)

instance Arrow QueryReader where
  arr f = Query $ \cs -> (mempty, cs, arr f)
  first (Query f) = Query $ \comps -> let (cIds, comps', qS) = f comps in (cIds, comps', first qS)

instance ArrowQueryReader QueryReader where
  entity = Query $ \cs -> (mempty, cs, entityDyn)
  fetch :: forall a. (Component a) => QueryReader () a
  fetch = Query $ \cs ->
    let (cId, cs') = CS.insert @a cs
     in (Set.singleton cId, cs', fetchDyn cId)
  fetchMaybe :: forall a. (Component a) => QueryReader () (Maybe a)
  fetchMaybe = Query $ \cs ->
    let (cId, cs') = CS.insert @a cs
     in (Set.singleton cId, cs', fetchMaybeDyn cId)

instance ArrowDynamicQueryReader QueryReader where
  entityDyn = Query $ \cs -> (mempty, cs, entityDyn)
  fetchDyn cId = Query $ \cs -> (Set.singleton cId, cs, fetchDyn cId)
  fetchMaybeDyn cId = Query $ \cs -> (Set.singleton cId, cs, fetchMaybeDyn cId)

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
