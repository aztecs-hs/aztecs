{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.Aztecs.System where

import Control.Arrow (Arrow (..), ArrowLoop (..))
import Control.Category (Category (..))
import Control.Monad.Fix (MonadFix)
import Data.Aztecs.Access (Access, runAccess)
import Data.Aztecs.Query (DynamicQuery, DynamicQueryFilter (..), Query (..), QueryFilter (..), ReadsWrites)
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World as W
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Components (ComponentID, Components)
import qualified Data.Foldable as F
import Data.Set (Set)
import Prelude hiding (all, id, map, (.))
import qualified Prelude

newtype System m i o = System
  {runSystem' :: Components -> (Components, ReadsWrites, DynamicSystem m i o)}
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

instance (Monad m) => Arrow (System m) where
  arr f = System (,mempty,arr f)
  first s = System $ \w -> let (w', cIds, dynS) = runSystem' s w in (w', cIds, first dynS)

instance (MonadFix m) => ArrowLoop (System m) where
  loop s = System $ \w -> let (w', cIds, dynS) = runSystem' s w in (w', cIds, loop dynS)

forever :: (Monad m) => System m () () -> System m () ()
forever s = System $ \w -> let (w', cIds, dynS) = runSystem' s w in (w', cIds, foreverDyn dynS)

runSystem_ :: (Monad m) => System m () () -> m ()
runSystem_ s = runSystem s >> pure ()

runSystem :: (Monad m) => System m () () -> m World
runSystem s = runSystemWorld s W.empty

runSystemWorld :: (Monad m) => System m () () -> World -> m World
runSystemWorld s w = do
  let (cs, _, dynS) = runSystem' s (components w)
      w' = w {components = cs}
  ((), w'', f', access) <- runSystemDyn dynS () w'
  ((), w''') <- runAccess access $ f' w''
  return w'''

all :: forall m a. (Monad m) => Query m () a -> System m () [a]
all q = System $ \cs ->
  let (rws, cs', qS) = runQuery q cs
   in (cs', rws, allDyn (Q.reads rws <> Q.writes rws) qS)

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
           in fmap (\(a, _) -> (a, w, Prelude.id, pure ())) (V.allDyn qS v)
      )

single :: forall m a. (Monad m) => Query m () a -> System m () a
single q =
  fmap
    ( \as -> case as of
        [a] -> a
        _ -> error "TODO"
    )
    (all q)

map :: forall m a. (Monad m) => Query m () a -> System m () [a]
map q = System $ \cs ->
  let (rws, cs', dynS) = runQuery q cs in (cs', rws, mapDyn (Q.reads rws <> Q.writes rws) dynS)

map_ :: forall m a. (Monad m) => Query m () a -> System m () ()
map_ q = const () <$> map q

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
           in fmap (\(a, v') -> (a, w, V.unview v', pure ())) (V.allDyn qS v)
      )

mapSingle :: forall m a. (Monad m) => Query m () a -> System m () a
mapSingle q =
  fmap
    ( \as -> case as of
        [a] -> a
        _ -> error "TODO"
    )
    (map q)

-- | Queue an `Access` to alter the world after this task is complete.
queue :: (Monad m) => (i -> Access m ()) -> System m i ()
queue f = System (,mempty,queueDyn f)

run :: (Monad m) => (i -> m o) -> System m i o
run f = System (,mempty,runDyn f)

newtype DynamicSystem m i o = DynamicSystem
  {runSystemDyn :: i -> World -> m (o, World, World -> World, Access m ())}
  deriving (Functor)

instance (Monad m) => Applicative (DynamicSystem m i) where
  pure a = DynamicSystem $ \_ w -> pure (a, w, Prelude.id, pure ())
  f <*> a =
    DynamicSystem $ \i w -> do
      (f'', w', fG, access) <- runSystemDyn f i w
      (a'', w'', aG, access') <- runSystemDyn a i w'
      return (f'' a'', w'', fG Prelude.. aG, access >> access')

instance (Monad m) => Category (DynamicSystem m) where
  id = DynamicSystem $ \i w -> pure (i, w, Prelude.id, pure ())
  f . g = DynamicSystem $ \i w -> do
    (a, w', g', access) <- runSystemDyn g i w
    ((), w'') <- runAccess access $ g' w'
    runSystemDyn f a w''

instance (Monad m) => Arrow (DynamicSystem m) where
  arr f = DynamicSystem $ \i w -> pure (f i, w, Prelude.id, pure ())
  first s =
    DynamicSystem $ \(i, x) w -> do
      (o, w', f, access) <- runSystemDyn s i w
      return ((o, x), w', f, access)

instance (MonadFix m) => ArrowLoop (DynamicSystem m) where
  loop s = DynamicSystem $ \b w -> mdo
    ((c, d), w', f, access) <- runSystemDyn s (b, d) (f w)
    return (c, w', f, access)

foreverDyn :: (Monad m) => DynamicSystem m () () -> DynamicSystem m () ()
foreverDyn s = DynamicSystem $ \_ w -> do
  let go wAcc = do
        ((), wAcc', f', access) <- runSystemDyn s () wAcc
        ((), wAcc'') <- runAccess access $ f' wAcc'
        go wAcc''
  go w

allDyn :: forall m a. (Monad m) => Set ComponentID -> DynamicQuery m () a -> DynamicSystem m () [a]
allDyn cIds q = DynamicSystem $ \_ w ->
  let v = V.view cIds (archetypes w)
   in fmap (\(a, _) -> (a, w, Prelude.id, pure ())) (V.allDyn q v)

singleDyn :: forall m a. (Monad m) => Set ComponentID -> DynamicQuery m () a -> DynamicSystem m () a
singleDyn cIds q =
  fmap
    ( \as -> case as of
        [a] -> a
        _ -> error "TODO"
    )
    (allDyn cIds q)

mapDyn :: forall m a. (Monad m) => Set ComponentID -> DynamicQuery m () a -> DynamicSystem m () [a]
mapDyn cIds q = DynamicSystem $ \_ w ->
  let v = V.view cIds (archetypes w)
   in fmap (\(a, v') -> (a, w, V.unview v', pure ())) (V.allDyn q v)

mapDyn_ :: forall m a. (Monad m) => Set ComponentID -> DynamicQuery m () a -> DynamicSystem m () ()
mapDyn_ cIds q = const () <$> mapDyn cIds q

queueDyn :: (Monad m) => (i -> Access m ()) -> DynamicSystem m i ()
queueDyn f = DynamicSystem $ \i w -> pure ((), w, Prelude.id, f i)

runDyn :: (Monad m) => (i -> m o) -> DynamicSystem m i o
runDyn f = DynamicSystem $ \i w -> do
  o <- f i
  return (o, w, Prelude.id, pure ())
