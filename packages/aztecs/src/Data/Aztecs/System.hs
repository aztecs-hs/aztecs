{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.Aztecs.System
  ( -- * Systems
    System (..),
    forever,
    run,
    queue,

    -- ** Queries
    all,
    filter,
    single,
    map,
    map_,
    mapSingle,
    filterMap,

    -- ** Running
    runSystem,
    runSystem_,
    runSystemWithWorld,

    -- * Dynamic systems
    DynamicSystem (..),
    allDyn,
    singleDyn,
    mapDyn,
    mapDyn_,
    queueDyn,
    runDyn,
  )
where

import Control.Arrow (Arrow (..), ArrowLoop (..))
import Control.Category (Category (..))
import Control.Concurrent (forkIO, putMVar, takeMVar)
import Control.Concurrent.MVar (newEmptyMVar)
import Data.Aztecs.Access (Access, runAccess)
import Data.Aztecs.Query (DynamicQuery, DynamicQueryFilter (..), Query (..), QueryFilter (..), ReadsWrites)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.View (View, unview)
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World as W
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Components (ComponentID, Components)
import qualified Data.Foldable as F
import Data.Set (Set)
import Prelude hiding (all, filter, id, map, (.))

-- | System that can access and alter a `World`.
--
-- Systems can be composed either in sequence or parallel with `Category` and `Arrow` combinators.
-- Using the arrow combinator `&&&`, systems will automatically run in parallel
-- as long as their queries don't intersect.
newtype System m i o = System
  { -- | Initialize a system, producing a `DynamicSystem`.
    runSystem' :: Components -> (Components, ReadsWrites, DynamicSystem m i o)
  }
  deriving (Functor)

instance (Monad m) => Applicative (System m i) where
  pure a = System (,mempty,pure a)
  f <*> a =
    System $ \w ->
      let (w', cIds, f') = runSystem' f w
          (w'', cIds', a') = runSystem' a w'
       in (w'', cIds <> cIds', f' <*> a')

instance (Monad m) => Category (System m) where
  id = System (,mempty,id)
  f . g = System $ \cs ->
    let (cs', cIds, g') = runSystem' g cs
        (cs'', cIds', f') = runSystem' f cs'
     in (cs'', cIds <> cIds', f' . g')

instance Arrow (System IO) where
  arr f = System (,mempty,arr f)
  first s = System $ \w -> let (w', cIds, dynS) = runSystem' s w in (w', cIds, first dynS)
  a &&& b = System $ \w ->
    let (w', aRws, dynA) = runSystem' a w
        (w'', bRws, dynB) = runSystem' b w'
        f = if Q.disjoint aRws bRws then (&&&) else joinDyn
     in (w'', aRws <> bRws, f dynA dynB)

instance ArrowLoop (System IO) where
  loop s = System $ \w -> let (w', cIds, dynS) = runSystem' s w in (w', cIds, loop dynS)

-- | Run a system forever.
forever :: (Monad m) => System m () () -> System m () ()
forever s = System $ \w -> let (w', cIds, dynS) = runSystem' s w in (w', cIds, foreverDyn dynS)

runSystem_ :: (Monad m) => System m () () -> m ()
runSystem_ s = runSystem s >> pure ()

runSystem :: (Monad m) => System m () () -> m World
runSystem s = runSystemWithWorld s W.empty

runSystemWithWorld :: (Monad m) => System m () () -> World -> m World
runSystemWithWorld s w = do
  let (cs, _, dynS) = runSystem' s (components w)
      w' = w {components = cs}
  ((), w'', _, access) <- runSystemDyn dynS () w'
  ((), _, w''') <- runAccess access w''
  return w'''

-- | Query all matching entities.
all :: forall m a. (Monad m) => Query m () a -> System m () [a]
all q = System $ \cs ->
  let (rws, cs', qS) = runQuery q cs
   in (cs', rws, allDyn (Q.reads rws <> Q.writes rws) qS)

-- | Query all matching entities with a `QueryFilter`.
filter :: forall m a. (Monad m) => Query m () a -> QueryFilter -> System m () [a]
filter q qf = System $ \cs ->
  let (rws, cs', qS) = runQuery q cs
      (dynQf, cs'') = runQueryFilter qf cs'
      f' arch =
        F.all (\cId -> A.member cId arch) (filterWith dynQf)
          && F.all (\cId -> not (A.member cId arch)) (filterWithout dynQf)
   in ( cs'',
        rws,
        DynamicSystem $ \_ w ->
          let v = V.filterView (Q.reads rws <> Q.writes rws) f' (archetypes w)
           in fmap (\(a, _) -> (a, w, mempty, pure ())) (V.allDyn qS v)
      )

-- | Query a single matching entity.
-- If there are zero or multiple matching entities, an error will be thrown.
single :: forall m a. (Monad m) => Query m () a -> System m () a
single q =
  fmap
    ( \as -> case as of
        [a] -> a
        _ -> error "TODO"
    )
    (all q)

-- | Map all matching entities, storing the updated entities.
map :: forall m a. (Monad m) => Query m () a -> System m () [a]
map q = System $ \cs ->
  let (rws, cs', dynS) = runQuery q cs in (cs', rws, mapDyn (Q.reads rws <> Q.writes rws) dynS)

-- | Map all matching entities and ignore the output, storing the updated entities.
map_ :: forall m a. (Monad m) => Query m () a -> System m () ()
map_ q = const () <$> map q

-- | Map all matching entities with a `QueryFilter`, storing the updated entities.
filterMap :: forall m a. (Monad m) => Query m () a -> QueryFilter -> System m () [a]
filterMap q qf = System $ \cs ->
  let (rws, cs', qS) = runQuery q cs
      (dynQf, cs'') = runQueryFilter qf cs'
      f' arch =
        F.all (\cId -> A.member cId arch) (filterWith dynQf)
          && F.all (\cId -> not (A.member cId arch)) (filterWithout dynQf)
   in ( cs'',
        rws,
        DynamicSystem $ \_ w ->
          let v = V.filterView (Q.reads rws <> Q.writes rws) f' (archetypes w)
           in fmap (\(a, v') -> (a, V.unview v' w, v', pure ())) (V.allDyn qS v)
      )

-- | Map a single matching entity, storing the updated components.
-- If there are zero or multiple matching entities, an error will be thrown.
mapSingle :: forall m a. (Monad m) => Query m () a -> System m () a
mapSingle q =
  fmap
    ( \as -> case as of
        [a] -> a
        _ -> error "TODO"
    )
    (map q)

-- | Queue an `Access` to alter the world after this system is complete.
queue :: (Monad m) => (i -> Access m ()) -> System m i ()
queue f = System (,mempty,queueDyn f)

-- | Run a monadic task.
run :: (Monad m) => (i -> m o) -> System m i o
run f = System (,mempty,runDyn f)

-- | Dynamic system that can access and alter a `World`.
newtype DynamicSystem m i o = DynamicSystem
  {runSystemDyn :: i -> World -> m (o, World, View, Access m ())}
  deriving (Functor)

instance (Monad m) => Applicative (DynamicSystem m i) where
  pure a = DynamicSystem $ \_ w -> pure (a, w, mempty, pure ())
  f <*> a =
    DynamicSystem $ \i w -> do
      (f'', w', v, access) <- runSystemDyn f i w
      (a'', w'', v', access') <- runSystemDyn a i w'
      return (f'' a'', w'', v <> v', access >> access')

instance (Monad m) => Category (DynamicSystem m) where
  id = DynamicSystem $ \i w -> pure (i, w, mempty, pure ())
  f . g = DynamicSystem $ \i w -> do
    (a, w', v, access) <- runSystemDyn g i w
    ((), v', w'') <- runAccess access w'
    (b, w''', v'', access') <- runSystemDyn f a w''
    return (b, w''', v <> v' <> v'', access')

instance Arrow (DynamicSystem IO) where
  arr f = DynamicSystem $ \i w -> pure (f i, w, mempty, pure ())
  first s =
    DynamicSystem $ \(i, x) w -> do
      (o, w', v, access) <- runSystemDyn s i w
      return ((o, x), w', v, access)

instance ArrowLoop (DynamicSystem IO) where
  loop s = DynamicSystem $ \b w -> mdo
    ((c, d), w', v, access) <- runSystemDyn s (b, d) w
    return (c, w', v, access)

-- | Combine two dynamic systems in parallel.
joinDyn :: DynamicSystem IO i a -> DynamicSystem IO i b -> DynamicSystem IO i (a, b)
joinDyn f g = DynamicSystem $ \i w -> do
  fVar <- newEmptyMVar
  gVar <- newEmptyMVar
  _ <- forkIO $ do
    result <- runSystemDyn f i w
    putMVar fVar result
  _ <- forkIO $ do
    result <- runSystemDyn g i w
    putMVar gVar result
  (a, _, v, accessF) <- takeMVar fVar
  (b, _, v', accessG) <- takeMVar gVar
  return ((a, b), unview (v <> v') w, v <> v', accessF >> accessG)

-- | Run a dynamic system forever.
foreverDyn :: (Monad m) => DynamicSystem m () () -> DynamicSystem m () ()
foreverDyn s = DynamicSystem $ \_ w -> do
  let go wAcc = do
        ((), wAcc', _, access) <- runSystemDyn s () wAcc
        ((), _, wAcc'') <- runAccess access wAcc'
        go wAcc''
  go w

-- | Query all matching entities.
allDyn :: forall m a. (Monad m) => Set ComponentID -> DynamicQuery m () a -> DynamicSystem m () [a]
allDyn cIds q = DynamicSystem $ \_ w ->
  let v = V.view cIds (archetypes w)
   in fmap (\(a, _) -> (a, w, mempty, pure ())) (V.allDyn q v)

-- | Query a single matching entity.
-- If there are zero or multiple matching entities, an error will be thrown.
singleDyn :: forall m a. (Monad m) => Set ComponentID -> DynamicQuery m () a -> DynamicSystem m () a
singleDyn cIds q =
  fmap
    ( \as -> case as of
        [a] -> a
        _ -> error "TODO"
    )
    (allDyn cIds q)

-- | Map all matching entities, storing the updated entities.
mapDyn :: forall m a. (Monad m) => Set ComponentID -> DynamicQuery m () a -> DynamicSystem m () [a]
mapDyn cIds q = DynamicSystem $ \_ w ->
  let v = V.view cIds (archetypes w)
   in fmap (\(a, v') -> (a, V.unview v' w, v', pure ())) (V.allDyn q v)

-- | Map all matching entities and ignore the output, storing the updated entities.
mapDyn_ :: forall m a. (Monad m) => Set ComponentID -> DynamicQuery m () a -> DynamicSystem m () ()
mapDyn_ cIds q = const () <$> mapDyn cIds q

-- | Queue an `Access` to alter the world after this system is complete.
queueDyn :: (Monad m) => (i -> Access m ()) -> DynamicSystem m i ()
queueDyn f = DynamicSystem $ \i w -> pure ((), w, mempty, f i)

-- | Run a monadic task.
runDyn :: (Monad m) => (i -> m o) -> DynamicSystem m i o
runDyn f = DynamicSystem $ \i w -> do
  o <- f i
  return (o, w, mempty, pure ())
