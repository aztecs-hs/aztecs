{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aztecs.Query
  ( Query (..),
    (<?>),
    fetch,
    fetchId,
    all,
    allWorld,
    map,
    mapWorld,
    Queryable (..),
  )
where

import Control.Monad.State (MonadState (..), gets)
import Data.Aztecs.Access (Access (Access))
import Data.Aztecs.Component
import Data.Aztecs.Entity (ConcatT, Difference, DifferenceT, Entity (..), EntityT, FromEntity (..), Intersect, IntersectT, ToEntity (..))
import qualified Data.Aztecs.Entity as E
import Data.Aztecs.World (World (..))
import Data.Aztecs.World.Archetype (Archetype)
import qualified Data.Aztecs.World.Archetype as A
import qualified Data.Aztecs.World.Archetypes as AS
import Data.Aztecs.World.Components (Components)
import qualified Data.Aztecs.World.Components as CS
import Data.Data (Typeable)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (all, map)

-- | Query into the `World`.
newtype Query a = Query
  { runQuery' ::
      Components ->
      ( Set ComponentID,
        Archetype ->
        ([Entity a], [Entity a] -> Archetype -> Archetype)
      )
  }

(<?>) ::
  (E.Split a (ConcatT a b), E.SplitT a (ConcatT a b) ~ b) =>
  Query a ->
  Query b ->
  Query (ConcatT a b)
(Query a) <?> (Query b) = Query $ \cs ->
  let (aIds, a') = a cs
      (bIds, b') = b cs
   in ( aIds <> bIds,
        \arch ->
          let (a'', aF) = a' arch
              (b'', bF) = b' arch
           in ( uncurry E.concat <$> zip a'' b'',
                \new newArch -> let (as, bs) = unzip $ fmap E.split new in bF bs $ aF as newArch
              )
      )

-- | Fetch a `Component` by its type.
fetch :: forall a. (Component a, Typeable (StorageT a)) => Query '[a]
fetch = Query $ \cs ->
  let cId = fromMaybe (error "TODO") (CS.lookup @a cs)
   in ( Set.singleton cId,
        \arch ->
          let as = A.all cId arch
           in ( fmap (\x -> ECons (snd x) ENil) as,
                A.insertAscList cId . fmap (\((e, _), ECons a ENil) -> (e, a)) . zip as
              )
      )

-- | Fetch a `Component` by its `ComponentID`.
fetchId :: forall a. (Component a, Typeable (StorageT a)) => ComponentID -> Query '[a]
fetchId cId = fetch' @a (const cId)

fetch' :: forall a. (Component a, Typeable (StorageT a)) => (Components -> ComponentID) -> Query '[a]
fetch' f = Query $ \cs ->
  let cId = f cs
   in ( Set.singleton cId,
        \arch ->
          let as = A.all cId arch
           in ( fmap (\x -> ECons (snd x) ENil) as,
                A.insertAscList cId . fmap (\((e, _), ECons a ENil) -> (e, a)) . zip as
              )
      )

all :: forall m a. (Monad m, ToEntity a, FromEntity a, Queryable (EntityT a)) => Access m [a]
all = Access $ gets (fmap fromEntity . allWorld' (query @(EntityT a)))

allWorld :: forall a. (ToEntity a, FromEntity a, Queryable (EntityT a)) => World -> [a]
allWorld = fmap fromEntity . allWorld' (query @(EntityT a))

allWorld' :: Query a -> World -> [Entity a]
allWorld' q w = fromMaybe [] $ do
  let (cIds, g) = runQuery' q (components w)
  return $ concatMap (fst . g) (AS.lookup cIds (archetypes w))

class Queryable a where
  query :: Query a

instance {-# OVERLAPPING #-} (Component a, Typeable (StorageT a)) => Queryable '[a] where
  query = fetch @a

instance (Component a, Typeable (StorageT a), Queryable as) => Queryable (a ': as) where
  query = fetch @a <?> query @as

-- | Map over all entities that match this query,
-- storing the resulting components in the @World@.
map ::
  forall m i o.
  (Monad m, Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o) =>
  (i -> o) ->
  Access m [o]
map f = Access $ do
  w <- get
  let (out, w') = mapWorld @i @o f w
  put w'
  return out

-- | Map over all entities that match this query,
-- storing the resulting components in the @World@.
mapWorld ::
  forall i o.
  (Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o) =>
  (i -> o) ->
  World ->
  ([o], World)
mapWorld = mapWorld' @(IsEq (Entity (EntityT i)) (Entity (EntityT o)))

-- Returns @True@ if @a@ and @b@ are the same type.
type family IsEq a b :: Bool where
  IsEq a a = 'True
  IsEq a b = 'False

-- Map over an entity's components.
--
-- @flag@ indicates if the input and output entities are the same,
-- in order to specialize at compile-time.
--
-- If the input and output entities are different, a reader and writer query are combined into one.
-- Otherwise, if the input and output entities are the same, we can use a more efficient implementation.
class Map (flag :: Bool) i o where
  mapWorld' :: (i -> o) -> World -> ([o], World)

instance (FromEntity i, ToEntity o, EntityT i ~ a, EntityT o ~ a, Queryable a) => Map 'True i o where
  mapWorld' f w = fromMaybe ([], w) $ do
    let (cIds, g) = runQuery' (query @a) (components w)
        go arch =
          let (as, h) = g arch
              as' = fmap (f . fromEntity) as
              arch' = h (fmap toEntity as') arch
           in (as', arch')
    let (es, arches) = AS.map cIds go (archetypes w)
    return (concat es, w {archetypes = arches})

instance
  ( FromEntity i,
    ToEntity o,
    Intersect (EntityT i) (EntityT o),
    Queryable (IntersectT (EntityT i) (EntityT o)),
    Difference (EntityT i) (EntityT o),
    Queryable (DifferenceT (EntityT i) (EntityT o)),
    ConcatT (DifferenceT (EntityT i) (EntityT o)) (IntersectT (EntityT i) (EntityT o)) ~ EntityT i,
    IntersectT (EntityT i) (EntityT o) ~ EntityT o
  ) =>
  Map 'False i o
  where
  mapWorld' f w = fromMaybe ([], w) $ do
    let i = query @(IntersectT (EntityT i) (EntityT o))
        o = query @(DifferenceT (EntityT i) (EntityT o))
        (aCIds, aG) = runQuery' i (components w)
        (bCIds, bG) = runQuery' o (components w)

    let g arch =
          let (as, aH) = aG arch
              (bs, _) = bG arch
              es = fmap (\(aE, bE) -> f $ fromEntity @i (E.concat bE aE)) (zip as bs)
              arch' = aH (fmap (\x -> let e = toEntity x in e) es) arch
           in (es, arch')
        (es', arches) = AS.map (aCIds <> bCIds) g (archetypes w)
    return (concat es', w {archetypes = arches})
