{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.Aztecs.System
  ( -- * Systems
    System,
    SystemT (..),
    forever,
    run,
    queue,

    -- ** Queries

    -- *** Reading
    all,
    filter,
    single,

    -- *** Writing
    map,
    map_,
    mapSingle,
    filterMap,

    -- ** Running
    runSystem,
    runSystem_,
    runSystemWithWorld,

    -- * Dynamic systems
    DynamicSystem,
    DynamicSystemT (..),
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
import Control.Concurrent (forkIO)
import Data.Aztecs.Access (Access, runAccess)
import Data.Aztecs.Query (DynamicQuery, DynamicQueryFilter (..), Query (..), QueryFilter (..), ReadsWrites)
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World as W
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Archetypes (Node (nodeArchetype))
import Data.Aztecs.World.Components (ComponentID, Components)
import qualified Data.Foldable as F
import Data.Set (Set)
import GHC.Conc (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Prelude hiding (all, filter, id, map, (.))

type System = SystemT IO

-- | System that can access and alter a `World`.
--
-- Systems can be composed either in sequence or parallel with `Category` and `Arrow` combinators.
-- Using the arrow combinator `&&&`, systems will automatically run in parallel
-- as long as their queries don't intersect.
newtype SystemT m i o = SystemT
  { -- | Initialize a system, producing a `DynamicSystem`.
    runSystem' :: Components -> (Components, ReadsWrites, DynamicSystemT m i o)
  }
  deriving (Functor)

instance (Monad m) => Applicative (SystemT m i) where
  pure a = SystemT (,mempty,pure a)
  f <*> a =
    SystemT $ \w ->
      let (w', cIds, f') = runSystem' f w
          (w'', cIds', a') = runSystem' a w'
       in (w'', cIds <> cIds', f' <*> a')

instance Category System where
  id = SystemT (,mempty,id)
  f . g = SystemT $ \cs ->
    let (cs', cIds, g') = runSystem' g cs
        (cs'', cIds', f') = runSystem' f cs'
     in (cs'', cIds <> cIds', f' . g')

instance Arrow System where
  arr f = SystemT (,mempty,arr f)
  first s = SystemT $ \w -> let (w', cIds, dynS) = runSystem' s w in (w', cIds, first dynS)
  a &&& b = SystemT $ \w ->
    let (w', aRws, dynA) = runSystem' a w
        (w'', bRws, dynB) = runSystem' b w'
        f = if Q.disjoint aRws bRws then (&&&) else joinDyn
     in (w'', aRws <> bRws, f dynA dynB)

instance ArrowLoop System where
  loop s = SystemT $ \w -> let (w', cIds, dynS) = runSystem' s w in (w', cIds, loop dynS)

-- | Run a system forever.
forever :: System () () -> System () ()
forever s = SystemT $ \w -> let (w', cIds, dynS) = runSystem' s w in (w', cIds, foreverDyn dynS)

runSystem_ :: System () () -> IO ()
runSystem_ s = runSystem s >> pure ()

runSystem :: System () () -> IO World
runSystem s = runSystemWithWorld s W.empty

runSystemWithWorld :: System () () -> World -> IO World
runSystemWithWorld s w = do
  let (cs, _, dynS) = runSystem' s (components w)
      w' = w {components = cs}
  wVar <- newTVarIO w'
  ((), access) <- runSystemDyn dynS () wVar
  w'' <- readTVarIO wVar
  ((), w''') <- runAccess access w''
  return w'''

-- | Query all matching entities.
all :: Query IO () a -> System () [a]
all q = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
   in (cs', rws, allDyn (Q.reads rws <> Q.writes rws) dynQ)

-- | Query all matching entities with a `QueryFilter`.
filter :: Query IO () a -> QueryFilter -> System () [a]
filter q qf = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
      (dynQf, cs'') = runQueryFilter qf cs'
      qf' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynQf)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynQf)
   in (cs'', rws, filterDyn (Q.reads rws <> Q.writes rws) dynQ qf')

-- | Query a single matching entity.
-- If there are zero or multiple matching entities, an error will be thrown.
single :: Query IO () a -> System () a
single q =
  fmap
    ( \as -> case as of
        [a] -> a
        _ -> error "TODO"
    )
    (all q)

-- | Map all matching entities, storing the updated entities.
map :: Query IO i a -> System i [a]
map q = SystemT $ \cs ->
  let (rws, cs', dynS) = runQuery q cs in (cs', rws, mapDyn (Q.reads rws <> Q.writes rws) dynS)

-- | Map all matching entities and ignore the output, storing the updated entities.
map_ :: Query IO i a -> System i ()
map_ q = const () <$> map q

-- | Map all matching entities with a `QueryFilter`, storing the updated entities.
filterMap :: Query IO i a -> QueryFilter -> System i [a]
filterMap q qf = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
      (dynQf, cs'') = runQueryFilter qf cs'
      f' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynQf)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynQf)
   in (cs'', rws, filterMapDyn (Q.reads rws <> Q.writes rws) dynQ f')

-- | Map a single matching entity, storing the updated components.
-- If there are zero or multiple matching entities, an error will be thrown.
mapSingle :: Query IO i a -> System i a
mapSingle q =
  fmap
    ( \as -> case as of
        [a] -> a
        _ -> error "TODO"
    )
    (map q)

-- | Queue an `Access` to alter the world after this system is complete.
queue :: (Monad m) => (i -> Access m ()) -> SystemT m i ()
queue f = SystemT (,mempty,queueDyn f)

-- | Run a monadic task.
run :: (Monad m) => (i -> m o) -> SystemT m i o
run f = SystemT (,mempty,runDyn f)

type DynamicSystem = DynamicSystemT IO

-- | Dynamic system that can access and alter a `World`.
newtype DynamicSystemT m i o = DynamicSystemT
  {runSystemDyn :: i -> TVar World -> m (o, Access m ())}
  deriving (Functor)

instance (Monad m) => Applicative (DynamicSystemT m i) where
  pure a = DynamicSystemT $ \_ _ -> pure (a, pure ())
  f <*> a =
    DynamicSystemT $ \i w -> do
      (f'', access) <- runSystemDyn f i w
      (a'', access') <- runSystemDyn a i w
      return (f'' a'', access >> access')

instance Category DynamicSystem where
  id = DynamicSystemT $ \i _ -> pure (i, pure ())
  f . g = DynamicSystemT $ \i wVar -> do
    (a, access) <- runSystemDyn g i wVar
    w <- readTVarIO wVar
    ((), w') <- runAccess access w
    atomically $ writeTVar wVar w'
    (b, access') <- runSystemDyn f a wVar
    return (b, access')

instance Arrow DynamicSystem where
  arr f = DynamicSystemT $ \i _ -> pure (f i, pure ())
  first s =
    DynamicSystemT $ \(i, x) w -> do
      (o, access) <- runSystemDyn s i w
      return ((o, x), access)

instance ArrowLoop DynamicSystem where
  loop s = DynamicSystemT $ \b w -> mdo
    ((c, d), access) <- runSystemDyn s (b, d) w
    return (c, access)

-- | Combine two dynamic systems in parallel.
joinDyn :: DynamicSystem i a -> DynamicSystem i b -> DynamicSystem i (a, b)
joinDyn f g = DynamicSystemT $ \i w -> do
  fVar <- newTVarIO Nothing
  gVar <- newTVarIO Nothing
  _ <- forkIO $ do
    result <- runSystemDyn f i w
    atomically $ writeTVar fVar (Just result)
  _ <- forkIO $ do
    result <- runSystemDyn g i w
    atomically $ writeTVar gVar (Just result)
  let go = do
        maybeA <- readTVarIO fVar
        maybeB <- readTVarIO gVar
        case (maybeA, maybeB) of
          (Just (a, accessA), Just (b, accessB)) -> return ((a, b), accessA >> accessB)
          _ -> go
  go

-- | Run a dynamic system forever.
foreverDyn :: DynamicSystem () () -> DynamicSystem () ()
foreverDyn s = DynamicSystemT $ \_ w -> do
  let go w' = do
        ((), access) <- runSystemDyn s () w'
        wAcc' <- readTVarIO w'
        ((), wAcc'') <- runAccess access wAcc'
        atomically $ writeTVar w' wAcc''
        go w'
  go w

-- | Query all matching entities.
allDyn :: Set ComponentID -> DynamicQuery IO () a -> DynamicSystem () [a]
allDyn cIds q = DynamicSystemT $ \_ w -> do
  w' <- readTVarIO w
  let v = V.view cIds (archetypes w')
  fmap (\(a, _) -> (a, pure ())) (V.allDyn () q v)

filterDyn :: Set ComponentID -> DynamicQuery IO i a -> (Node -> Bool) -> DynamicSystemT IO i [a]
filterDyn cIds q f = DynamicSystemT $ \i wVar -> do
  w <- readTVarIO wVar
  let v = V.filterView cIds f (archetypes w)
  (as, _) <- V.allDyn i q v
  return (as, pure ())

-- | Query a single matching entity.
-- If there are zero or multiple matching entities, an error will be thrown.
singleDyn :: Set ComponentID -> DynamicQuery IO () a -> DynamicSystem () a
singleDyn cIds q =
  fmap
    ( \as -> case as of
        [a] -> a
        _ -> error "TODO"
    )
    (allDyn cIds q)

-- | Map all matching entities, storing the updated entities.
mapDyn :: Set ComponentID -> DynamicQuery IO i a -> DynamicSystem i [a]
mapDyn cIds q = DynamicSystemT $ \i wVar -> do
  w <- readTVarIO wVar
  let v = V.view cIds (archetypes w)
  (as, v') <- V.allDyn i q v
  atomically $ writeTVar wVar $ V.unview v' w
  return (as, pure ())

filterMapDyn :: Set ComponentID -> DynamicQuery IO i a -> (Node -> Bool) -> DynamicSystemT IO i [a]
filterMapDyn cIds q f = DynamicSystemT $ \i wVar -> do
  w <- readTVarIO wVar
  let v = V.filterView cIds f (archetypes w)
  (as, v') <- V.allDyn i q v
  atomically $ writeTVar wVar $ V.unview v' w
  return (as, pure ())

-- | Map all matching entities and ignore the output, storing the updated entities.
mapDyn_ :: Set ComponentID -> DynamicQuery IO () a -> DynamicSystem () ()
mapDyn_ cIds q = const () <$> mapDyn cIds q

-- | Queue an `Access` to alter the world after this system is complete.
queueDyn :: (Applicative m) => (i -> Access m ()) -> DynamicSystemT m i ()
queueDyn f = DynamicSystemT $ \i _ -> pure ((), f i)

-- | Run a monadic task.
runDyn :: (Monad m) => (i -> m o) -> DynamicSystemT m i o
runDyn f = DynamicSystemT $ \i _ -> fmap (,pure ()) (f i)
