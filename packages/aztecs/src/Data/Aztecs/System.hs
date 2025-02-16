{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Data.Aztecs.System
  ( System (..),
    all,
    map,
    queue,
    task,
    DynamicSystem (..),
    allDyn,
    mapDyn,
    queueDyn,
    raceDyn,
    Schedule (..),
    schedule,
    forever,
    runSchedule,
    runSchedule_,
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Concurrent.ParallelIO.Global
import Control.Monad ((>=>))
import Control.Monad.State (MonadState (..), MonadTrans (..))
import Data.Aztecs.Access (Access (..), runAccess)
import Data.Aztecs.Component (ComponentID)
import Data.Aztecs.Query (DynamicQuery, Query (..), ReadsWrites)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.View (View)
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World as W
import Data.Aztecs.World.Components (Components)
import Data.Set (Set)
import Prelude hiding (all, map, (.))
import qualified Prelude hiding (map)

newtype System m i o = System {runSystem :: Components -> (DynamicSystem m i o, ReadsWrites, Components)}
  deriving (Functor)

instance (Monad m) => Category (System m) where
  id = System $ \cs -> (DynamicSystem $ \_ -> \i -> return (i, mempty, pure ()), mempty, cs)
  System f . System g = System $ \cs ->
    let (f', rwsF, cs') = f cs
        (g', rwsG, cs'') = g cs'
     in (f' . g', rwsF <> rwsG, cs'')

instance Arrow (System IO) where
  arr f = System $ \cs -> (DynamicSystem $ \_ -> \i -> return (f i, mempty, pure ()), mempty, cs)
  first (System f) = System $ \cs ->
    let (f', rwsF, cs') = f cs
     in (first f', rwsF, cs')
  a &&& b = System $ \cs ->
    let (dynA, rwsA, cs') = runSystem a cs
        (dynB, rwsB, cs'') = runSystem b cs'
     in ( if Q.disjoint rwsA rwsB
            then dynA &&& dynB
            else raceDyn dynA dynB,
          rwsA <> rwsB,
          cs''
        )

-- | Query all matching entities.
all :: (Monad m) => Query m i a -> System m i [a]
all q = System $ \cs ->
  let !(rws, cs', dynQ) = runQuery q cs
   in (allDyn (Q.reads rws <> Q.writes rws) dynQ, rws, cs')

-- | Query all matching entities.
map :: (Monad m) => Query m i a -> System m i [a]
map q = System $ \cs ->
  let !(rws, cs', dynQ) = runQuery q cs
   in (mapDyn (Q.reads rws <> Q.writes rws) dynQ, rws, cs')

queue :: (Monad m) => (i -> Access m ()) -> System m i ()
queue f = System $ \cs -> (queueDyn f, mempty, cs)

task :: (Monad m) => (i -> m ()) -> System m i ()
task f = System $ \cs -> (DynamicSystem $ \_ -> \i -> f i >> return ((), mempty, pure ()), mempty, cs)

newtype DynamicSystem m i o = DynamicSystem {runSystemDyn :: World -> (i -> m (o, View, Access m ()))}
  deriving (Functor)

instance (Monad m) => Category (DynamicSystem m) where
  id = DynamicSystem $ \_ -> \i -> return (i, mempty, pure ())
  DynamicSystem f . DynamicSystem g = DynamicSystem $ \w -> \i -> do
    (b, gView, gAccess) <- g w i
    (a, fView, fAccess) <- f w b
    return (a, gView <> fView, gAccess >> fAccess)

instance (Monad m) => Arrow (DynamicSystem m) where
  arr f = DynamicSystem $ \_ -> \i -> return (f i, mempty, pure ())
  first (DynamicSystem f) = DynamicSystem $ \w -> \(i, x) -> do
    (a, v, access) <- f w i
    return ((a, x), v, access)

-- | Query all matching entities.
allDyn :: (Monad m) => Set ComponentID -> DynamicQuery m i o -> DynamicSystem m i [o]
allDyn cIds q = DynamicSystem $ \w ->
  let !v = V.view cIds $ archetypes w
   in \i -> fmap (\(a, _) -> (a, mempty, pure ())) (V.allDyn i q v)

-- | Map all matching entities, storing the updated entities.
mapDyn :: (Monad m) => Set ComponentID -> DynamicQuery m i o -> DynamicSystem m i [o]
mapDyn cIds q = DynamicSystem $ \w ->
  let !v = V.view cIds $ archetypes w
   in \i -> fmap (\(a, v') -> (a, v', pure ())) (V.allDyn i q v)

queueDyn :: (Monad m) => (i -> Access m ()) -> DynamicSystem m i ()
queueDyn f = DynamicSystem $ \_ -> \i -> return ((), mempty, f i)

raceDyn :: DynamicSystem IO i a -> DynamicSystem IO i b -> DynamicSystem IO i (a, b)
raceDyn (DynamicSystem f) (DynamicSystem g) = DynamicSystem $ \w -> \i -> do
  results <- parallel [fmap (\a -> (Just a, Nothing)) $ f w i, fmap (\b -> (Nothing, Just b)) $ g w i]
  ((a, v, fAccess), (b, v', gAccess)) <- case results of
    [(Just a, _), (_, Just b)] -> return (a, b)
    _ -> error "joinDyn: exception"
  return ((a, b), v <> v', fAccess >> gAccess)

newtype Schedule m i o = Schedule {runSchedule' :: Components -> (i -> Access m o, Components)}

instance (Monad m) => Category (Schedule m) where
  id = Schedule $ \cs -> (return, cs)
  Schedule f . Schedule g = Schedule $ \cs ->
    let (g', cs') = g cs
        (f', cs'') = f cs'
     in (g' >=> f', cs'')

instance Arrow (Schedule IO) where
  arr f = Schedule $ \cs -> (return . f, cs)
  first (Schedule f) = Schedule $ \cs ->
    let (g, cs') = f cs
     in (\(b, d) -> (,) <$> g b <*> return d, cs')

runSchedule :: (Monad m) => Schedule m i o -> World -> i -> m (o, World)
runSchedule s w i = do
  let (f, cs) = runSchedule' s (components w)
  (o, w') <- runAccess (f i) w {components = cs}
  return (o, w')

runSchedule_ :: (Monad m) => Schedule m () () -> m ()
runSchedule_ s = const () <$> runSchedule s (W.empty) ()

schedule :: (Monad m) => System m i o -> Schedule m i o
schedule t = Schedule $ \cs ->
  let (dynT, _, cs') = runSystem t cs
      go i = Access $ do
        w <- get
        let f = runSystemDyn dynT w
        (o, v, access) <- lift $ f i
        ((), w') <- lift Prelude.. runAccess access $ V.unview v w
        put w'
        return o
   in (go, cs')

forever :: (Monad m) => Schedule m i () -> Schedule m i ()
forever s = Schedule $ \cs ->
  let (f, cs') = runSchedule' s cs
      go i = Access $ do
        w <- get
        let loop wAcc = do
              ((), wAcc') <- lift $ runAccess (f i) wAcc
              loop wAcc'
        loop w
   in (go, cs')
