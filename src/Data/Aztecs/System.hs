{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.System where

import Control.Arrow (Arrow (..), ArrowLoop (..))
import Control.Category (Category (..))
import Control.Monad.Fix (MonadFix)
import Data.Aztecs.Access (Access, runAccess)
import Data.Aztecs.Query (Query (..), QueryFilter (..))
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World as W
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Components (ComponentID, Components)
import qualified Data.Foldable as F
import Data.Set (Set)
import Prelude hiding (map, all)

newtype System m i o = System
  { runSystem' ::
      Components ->
      ( Components,
        [Set ComponentID],
        i -> World -> m (o, World, World -> World, Access m ())
      )
  }
  deriving (Functor)

instance (Monad m) => Applicative (System m i) where
  pure a = System (,[],\_ w -> pure (a, w, Prelude.id, pure ()))
  f <*> a =
    System $ \w ->
      let (w', cIds, f') = runSystem' f w
          (w'', cIds', a') = runSystem' a w'
       in ( w'',
            cIds <> cIds',
            \i wAcc -> do
              (f'', wAcc', fG, access) <- f' i wAcc
              (a'', wAcc'', aG, access') <- a' i wAcc'
              return (f'' a'', wAcc'', fG Prelude.. aG, access >> access')
          )

instance (Monad m) => Category (System m) where
  id = System (,[],\i w -> pure (i, w, Prelude.id, pure ()))
  (.) t1 t2 = System $ \w ->
    let (w', cIds, f) = runSystem' t2 w
        (w'', cIds', g) = runSystem' t1 w'
     in ( w'',
          cIds <> cIds',
          \i wAcc -> do
            (o, wAcc', f', access) <- f i wAcc
            ((), wAcc'') <- runAccess access $ f' wAcc'
            (a, wAcc''', g', access') <- g o wAcc''
            ((), wAcc'''') <- runAccess access' $ g' wAcc'''
            return (a, wAcc'''', Prelude.id, pure ())
        )

instance (Monad m) => Arrow (System m) where
  arr f = System (,[],\i w -> pure (f i, w, Prelude.id, pure ()))
  first t =
    System $ \w ->
      let (w', cIds, f) = runSystem' t w
       in ( w',
            cIds,
            \(i, x) w'' -> do
              (o, w''', f', access) <- f i w''
              ((), w'''') <- runAccess access $ f' w'''
              return ((o, x), w'''', f', pure ())
          )

instance (MonadFix m) => ArrowLoop (System m) where
  loop s = System $ \w ->
    let (w', cIds, f) = runSystem' s w
     in ( w',
          cIds,
          \b w'' -> mdo
            ((c, d), w''', f', access) <- f (b, d) w''
            ((), w'''') <- runAccess access $ f' w'''
            return (c, w'''', f', access)
        )

forever :: (Monad m) => System m () () -> System m () ()
forever s = System $ \w ->
  let (w', cIds, f) = runSystem' s w
   in ( w',
        cIds,
        \_ w'' -> do
          let go wAcc = do
                ((), wAcc', f', access) <- f () wAcc
                ((), wAcc'') <- runAccess access $ f' wAcc'
                go wAcc''
          go w''
      )

runSystem_ :: (Monad m) => System m () () -> m ()
runSystem_ s = runSystem s >> pure ()

runSystem :: (Monad m) => System m () () -> m World
runSystem s = runSystemWorld s W.empty

runSystemWorld :: (Monad m) => System m () () -> World -> m World
runSystemWorld s w = do
  let (cs, _, f) = runSystem' s (components w)
      w' = w {components = cs}
  ((), w'', f', access) <- f () w'
  ((), w''') <- runAccess access $ f' w''
  return w'''

all :: forall m a. (Monad m) => Query m () a -> System m () [a]
all q = System $ \cs ->
  let (cIds, cs', qS) = runQuery q cs
   in ( cs',
        [cIds],
        \_ w ->
          let v = V.view cIds (archetypes w)
           in fmap (\(a, _) -> (a, w, Prelude.id, pure ())) (V.allState qS v)
      )

filter :: forall m a. (Monad m) => Query m () a -> QueryFilter -> System m () [a]
filter q f = System $ \cs ->
  let (cIds, cs', qS) = runQuery q cs
      (with', cs'') = filterWith f cs'
      (without', cs''') = filterWithout f cs''
      f' arch =
        F.all (\cId -> A.member cId arch) with'
          && F.all (\cId -> not (A.member cId arch)) without'
   in ( cs''',
        [cIds],
        \_ w ->
          let v = V.viewFilter cIds f' (archetypes w)
           in fmap (\(a, _) -> (a, w, Prelude.id, pure ())) (V.allState qS v)
      )

single :: forall m a. (Monad m) => Query m () a -> System m () a
single q =
  fmap
    ( \as -> case as of
        [a] -> a
        _ -> error "TODO"
    )
    (map q)

map :: forall m a. (Monad m) => Query m () a -> System m () [a]
map q = System $ \cs ->
  let (cIds, cs', qS) = runQuery q cs
   in ( cs',
        [cIds],
        \_ w ->
          let v = V.view cIds (archetypes w)
           in fmap (\(a, v') -> (a, w, V.unview v', pure ())) (V.allState qS v)
      )

map_ :: forall m a. (Monad m) => Query m () a -> System m () ()
map_ q = const () <$> map q

filterMap :: forall m a. (Monad m) => Query m () a -> QueryFilter -> System m () [a]
filterMap q f = System $ \cs ->
  let (cIds, cs', qS) = runQuery q cs
      (with', cs'') = filterWith f cs'
      (without', cs''') = filterWithout f cs''
      f' arch =
        F.all (\cId -> A.member cId arch) with'
          && F.all (\cId -> not (A.member cId arch)) without'
   in ( cs''',
        [cIds],
        \_ w ->
          let v = V.viewFilter cIds f' (archetypes w)
           in fmap (\(a, v') -> (a, w, V.unview v', pure ())) (V.allState qS v)
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
queue :: (Monad m) => Access m () -> System m () ()
queue a = System (,[],\_ w -> pure ((), w, Prelude.id, a))

queueWith :: (Monad m) => (i -> Access m ()) -> System m i ()
queueWith f = System (,[],\i w -> pure ((), w, Prelude.id, f i))

run :: (Monad m) => (i -> m o) -> System m i o
run f =
  System
    (,[],\i w -> do
           o <- f i
           return (o, w, Prelude.id, pure ()))
