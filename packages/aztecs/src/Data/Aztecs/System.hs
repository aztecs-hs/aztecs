{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aztecs.System
  ( Task (..),
    all,
    DynamicTask (..),
    allDyn,
    raceDyn,
    System,
    task,
    runSystem,
    runSystem_,
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Concurrent.ParallelIO.Global
import Control.Monad.State (MonadState (..), MonadTrans (..))
import Data.Aztecs (Access, runAccess)
import Data.Aztecs.Access (Access (Access))
import Data.Aztecs.Component (ComponentID)
import Data.Aztecs.Query (DynamicQuery, Query (..), ReadsWrites)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.View (View)
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import Data.Aztecs.World.Components (Components)
import Data.Set (Set)
import Prelude hiding (all, (.))
import qualified Prelude

newtype Task m i o = Task {runTask :: Components -> (DynamicTask m i o, ReadsWrites, Components)}
  deriving (Functor)

instance (Monad m) => Category (Task m) where
  id = Task $ \cs -> (DynamicTask $ \_ -> \i -> return (i, mempty, pure ()), mempty, cs)
  Task f . Task g = Task $ \cs ->
    let (f', rwsF, cs') = f cs
        (g', rwsG, cs'') = g cs'
     in (f' . g', rwsF <> rwsG, cs'')

instance Arrow (Task IO) where
  arr f = Task $ \cs -> (DynamicTask $ \_ -> \i -> return (f i, mempty, pure ()), mempty, cs)
  first (Task f) = Task $ \cs ->
    let (f', rwsF, cs') = f cs
     in (first f', rwsF, cs')
  a &&& b = Task $ \cs ->
    let (dynA, rwsA, cs') = runTask a cs
        (dynB, rwsB, cs'') = runTask b cs'
     in ( if Q.disjoint rwsA rwsB
            then dynA &&& dynB
            else raceDyn dynA dynB,
          rwsA <> rwsB,
          cs''
        )

-- | Query all matching entities.
all :: (Monad m) => Query m i a -> Task m i [a]
all q = Task $ \cs ->
  let !(rws, cs', dynQ) = runQuery q cs
   in (allDyn (Q.reads rws <> Q.writes rws) dynQ, rws, cs')

newtype DynamicTask m i o = DynamicTask {runTaskDyn :: World -> (i -> m (o, View, Access m ()))}
  deriving (Functor)

instance (Monad m) => Category (DynamicTask m) where
  id = DynamicTask $ \_ -> \i -> return (i, mempty, pure ())
  DynamicTask f . DynamicTask g = DynamicTask $ \w -> \i -> do
    (b, gView, gAccess) <- g w i
    (a, fView, fAccess) <- f w b
    return (a, gView <> fView, gAccess >> fAccess)

instance (Monad m) => Arrow (DynamicTask m) where
  arr f = DynamicTask $ \_ -> \i -> return (f i, mempty, pure ())
  first (DynamicTask f) = DynamicTask $ \w -> \(i, x) -> do
    (a, v, access) <- f w i
    return ((a, x), v, access)

-- | Query all matching entities.
allDyn :: (Monad m) => Set ComponentID -> DynamicQuery m i o -> DynamicTask m i [o]
allDyn cIds q = DynamicTask $ \w ->
  let !v = V.view cIds $ archetypes w
   in \i -> fmap (\(a, _) -> (a, mempty, pure ())) (V.allDyn i q v)

raceDyn :: DynamicTask IO i a -> DynamicTask IO i b -> DynamicTask IO i (a, b)
raceDyn (DynamicTask f) (DynamicTask g) = DynamicTask $ \w -> \i -> do
  results <- parallel [fmap (\a -> (Just a, Nothing)) $ f w i, fmap (\b -> (Nothing, Just b)) $ g w i]
  ((a, v, fAccess), (b, v', gAccess)) <- case results of
    [(Just a, _), (_, Just b)] -> return (a, b)
    _ -> error "joinDyn: exception"
  return ((a, b), v <> v', fAccess >> gAccess)

newtype System m i o = System {runSystem' :: Components -> (i -> Access m o, Components)}

runSystem :: (Monad m) => System m i o -> World -> i -> m (o, World)
runSystem s w i = do
  let (f, cs) = runSystem' s (components w)
  (o, w') <- runAccess (f i) w {components = cs}
  return (o, w')

runSystem_ :: (Monad m) => System m () () -> World -> m ()
runSystem_ s w = const () <$> runSystem s w ()

task :: (Monad m) => Task m i o -> System m i o
task t = System $ \cs ->
  let (dynT, _, cs') = runTask t cs
      go i = Access $ do
        w <- get
        let f = runTaskDyn dynT w
        (o, v, access) <- lift $ f i
        ((), w') <- lift Prelude.. runAccess access $ V.unview v w
        put w'
        return o
   in (go, cs')
