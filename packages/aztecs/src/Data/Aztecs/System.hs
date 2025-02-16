{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Data.Aztecs.System
  ( -- * Systems
    System,
    SystemT (..),
    queue,
    task,

    -- ** Queries

    -- *** Reading
    all,
    filter,
    single,

    -- *** Writing
    map,
    map_,
    filterMap,
    mapSingle,

    -- * Dynamic SystemTs
    DynamicSystemT (..),
    queueDyn,
    raceDyn,

    -- ** Queries

    -- *** Reading
    allDyn,
    filterDyn,
    singleDyn,

    -- *** Writing
    mapDyn,
    filterMapDyn,
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Concurrent.ParallelIO.Global
import Data.Aztecs.Access (Access (..))
import Data.Aztecs.Component (ComponentID)
import Data.Aztecs.Query (DynamicQuery, DynamicQueryFilter (..), Query (..), QueryFilter (..), ReadsWrites)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.View (View)
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Archetypes (Node (..))
import Data.Aztecs.World.Components (Components)
import qualified Data.Foldable as F
import Data.Set (Set)
import Prelude hiding (all, filter, map, (.))
import qualified Prelude hiding (filter, map)

type System i o = SystemT IO i o

newtype SystemT m i o = SystemT {runSystemT :: Components -> (DynamicSystemT m i o, ReadsWrites, Components)}
  deriving (Functor)

instance (Monad m) => Category (SystemT m) where
  id = SystemT $ \cs -> (DynamicSystemT $ \_ -> \i -> return (i, mempty, pure ()), mempty, cs)
  SystemT f . SystemT g = SystemT $ \cs ->
    let (f', rwsF, cs') = f cs
        (g', rwsG, cs'') = g cs'
     in (f' . g', rwsF <> rwsG, cs'')

instance Arrow (SystemT IO) where
  arr f = SystemT $ \cs -> (DynamicSystemT $ \_ -> \i -> return (f i, mempty, pure ()), mempty, cs)
  first (SystemT f) = SystemT $ \cs ->
    let (f', rwsF, cs') = f cs
     in (first f', rwsF, cs')
  a &&& b = SystemT $ \cs ->
    let (dynA, rwsA, cs') = runSystemT a cs
        (dynB, rwsB, cs'') = runSystemT b cs'
     in ( if Q.disjoint rwsA rwsB
            then dynA &&& dynB
            else raceDyn dynA dynB,
          rwsA <> rwsB,
          cs''
        )

-- | Query all matching entities.
all :: (Monad m) => Query m i a -> SystemT m i [a]
all q = SystemT $ \cs ->
  let !(rws, cs', dynQ) = runQuery q cs
   in (allDyn (Q.reads rws <> Q.writes rws) dynQ, rws, cs')

-- | Query all matching entities with a `QueryFilter`.
filter :: (Monad m) => Query m () a -> QueryFilter -> SystemT m () [a]
filter q qf = SystemT $ \cs ->
  let !(rws, cs', dynQ) = runQuery q cs
      !(dynQf, cs'') = runQueryFilter qf cs'
      qf' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynQf)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynQf)
   in (filterDyn (Q.reads rws <> Q.writes rws) dynQ qf', rws, cs'')

-- | Query a single matching entity.
-- If there are zero or multiple matching entities, an error will be thrown.
single :: (Monad m) => Query m () a -> SystemT m () a
single q =
  fmap
    ( \as -> case as of
        [a] -> a
        _ -> error "TODO"
    )
    (all q)

-- | Query all matching entities.
map :: (Monad m) => Query m i a -> SystemT m i [a]
map q = SystemT $ \cs ->
  let !(rws, cs', dynQ) = runQuery q cs
   in (mapDyn (Q.reads rws <> Q.writes rws) dynQ, rws, cs')

map_ :: (Monad m) => Query m i o -> SystemT m i ()
map_ q = const () <$> map q

-- | Map all matching entities with a `QueryFilter`, storing the updated entities.
filterMap :: (Monad m) => Query m i a -> QueryFilter -> SystemT m i [a]
filterMap q qf = SystemT $ \cs ->
  let !(rws, cs', dynQ) = runQuery q cs
      !(dynQf, cs'') = runQueryFilter qf cs'
      f' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynQf)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynQf)
   in (filterMapDyn (Q.reads rws <> Q.writes rws) dynQ f', rws, cs'')

-- | Map a single matching entity, storing the updated components.
-- If there are zero or multiple matching entities, an error will be thrown.
mapSingle :: (Monad m) => Query m i a -> SystemT m i a
mapSingle q =
  fmap
    ( \as -> case as of
        [a] -> a
        _ -> error "TODO"
    )
    (map q)

queue :: (Monad m) => (i -> Access m ()) -> SystemT m i ()
queue f = SystemT $ \cs -> (queueDyn f, mempty, cs)

task :: (Monad m) => (i -> m o) -> SystemT m i o
task f = SystemT $ \cs ->
  ( DynamicSystemT $ \_ -> \i -> do
      o <- f i
      return (o, mempty, pure ()),
    mempty,
    cs
  )

newtype DynamicSystemT m i o = DynamicSystemT {runSystemTDyn :: World -> (i -> m (o, View, Access m ()))}
  deriving (Functor)

instance (Monad m) => Category (DynamicSystemT m) where
  id = DynamicSystemT $ \_ -> \i -> return (i, mempty, pure ())
  DynamicSystemT f . DynamicSystemT g = DynamicSystemT $ \w -> \i -> do
    (b, gView, gAccess) <- g w i
    (a, fView, fAccess) <- f w b
    return (a, gView <> fView, gAccess >> fAccess)

instance (Monad m) => Arrow (DynamicSystemT m) where
  arr f = DynamicSystemT $ \_ -> \i -> return (f i, mempty, pure ())
  first (DynamicSystemT f) = DynamicSystemT $ \w -> \(i, x) -> do
    (a, v, access) <- f w i
    return ((a, x), v, access)

-- | Query all matching entities.
allDyn :: (Monad m) => Set ComponentID -> DynamicQuery m i o -> DynamicSystemT m i [o]
allDyn cIds q = DynamicSystemT $ \w ->
  let !v = V.view cIds $ archetypes w
   in \i -> fmap (\(a, _) -> (a, mempty, pure ())) (V.allDyn i q v)

filterDyn :: (Monad m) => Set ComponentID -> DynamicQuery m i o -> (Node -> Bool) -> DynamicSystemT m i [o]
filterDyn cIds q f = DynamicSystemT $ \w ->
  let !v = V.filterView cIds f $ archetypes w
   in \i -> fmap (\(a, _) -> (a, mempty, pure ())) (V.allDyn i q v)

singleDyn :: (Monad m) => Set ComponentID -> DynamicQuery m () a -> DynamicSystemT m () a
singleDyn cIds q =
  fmap
    ( \as -> case as of
        [a] -> a
        _ -> error "TODO"
    )
    (allDyn cIds q)

-- | Map all matching entities, storing the updated entities.
mapDyn :: (Monad m) => Set ComponentID -> DynamicQuery m i o -> DynamicSystemT m i [o]
mapDyn cIds q = DynamicSystemT $ \w ->
  let !v = V.view cIds $ archetypes w
   in \i -> fmap (\(a, v') -> (a, v', pure ())) (V.allDyn i q v)

filterMapDyn :: (Monad m) => Set ComponentID -> DynamicQuery m i o -> (Node -> Bool) -> DynamicSystemT m i [o]
filterMapDyn cIds q f = DynamicSystemT $ \w ->
  let !v = V.filterView cIds f $ archetypes w
   in \i -> fmap (\(a, v') -> (a, v', pure ())) (V.allDyn i q v)

queueDyn :: (Monad m) => (i -> Access m ()) -> DynamicSystemT m i ()
queueDyn f = DynamicSystemT $ \_ -> \i -> return ((), mempty, f i)

raceDyn :: DynamicSystemT IO i a -> DynamicSystemT IO i b -> DynamicSystemT IO i (a, b)
raceDyn (DynamicSystemT f) (DynamicSystemT g) = DynamicSystemT $ \w -> \i -> do
  results <- parallel [fmap (\a -> (Just a, Nothing)) $ f w i, fmap (\b -> (Nothing, Just b)) $ g w i]
  ((a, v, fAccess), (b, v', gAccess)) <- case results of
    [(Just a, _), (_, Just b)] -> return (a, b)
    _ -> error "joinDyn: exception"
  return ((a, b), v <> v', fAccess >> gAccess)
