{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.System where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Monad.Reader (MonadIO, MonadReader (..), MonadTrans (..), ReaderT (runReaderT))
import Control.Monad.Writer (MonadWriter (..), WriterT (runWriterT))
import Data.Aztecs.Access (Access, runAccess)
import Data.Aztecs.Entity (ComponentIds (componentIds), Entity, EntityID, EntityT, FromEntity)
import Data.Aztecs.Query (IsEq, QueryFilter (..), Queryable)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.View (View (..))
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (ArchetypeID, World (..))
import qualified Data.Aztecs.World as W
import Data.Aztecs.World.Archetype (Archetype, Lookup)
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Components (ComponentID, Components)
import qualified Data.Foldable as F
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Prelude hiding (all)

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
            (a, wAcc'', g', access') <- g o wAcc'
            ((), wAcc''') <- runAccess (access >> access') $ g' (g' $ f' wAcc'')
            return (a, wAcc''', Prelude.id, pure ())
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

loop :: (Monad m) => System m () () -> System m () ()
loop s = System $ \w ->
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

all :: forall m a. (Monad m, FromEntity a, ComponentIds (EntityT a), Queryable (EntityT a)) => System m () [(EntityID, a)]
all = view @_ @(EntityT a) allView

allFilter ::
  forall m a.
  (Monad m, FromEntity a, ComponentIds (EntityT a), Queryable (EntityT a)) =>
  QueryFilter ->
  System m () [(EntityID, a)]
allFilter f = viewFilter @_ @(EntityT a) f allView

single ::
  forall m a.
  ( Monad m,
    FromEntity a,
    ComponentIds (EntityT a),
    Queryable (EntityT a)
  ) =>
  System m () a
single = fmap (fromMaybe (error "TODO")) maybeSingle

maybeSingle ::
  forall m a.
  ( Monad m,
    FromEntity a,
    ComponentIds (EntityT a),
    Queryable (EntityT a)
  ) =>
  System m () (Maybe a)
maybeSingle = fmap (fmap snd) maybeSingleWithId

maybeSingleWithId ::
  forall m a.
  ( Monad m,
    FromEntity a,
    ComponentIds (EntityT a),
    Queryable (EntityT a)
  ) =>
  System m () (Maybe (EntityID, a))
maybeSingleWithId =
  fmap
    ( \xs -> case xs of
        [(eId, a)] -> Just (eId, a)
        _ -> Nothing
    )
    all

lookup ::
  forall m a.
  ( Monad m,
    FromEntity a,
    ComponentIds (EntityT a),
    Queryable (EntityT a),
    Lookup (Entity (EntityT a))
  ) =>
  EntityID ->
  System m () (Maybe a)
lookup eId = view @_ @(EntityT a) (lookupView eId)

map ::
  forall m i o.
  ( Monad m,
    ComponentIds (EntityT i),
    Queryable (EntityT i),
    Q.Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o
  ) =>
  (i -> o) ->
  System m () [o]
map f = mapView (\v cs -> pure $ V.map f v cs)

mapM ::
  forall m i o.
  ( Monad m,
    ComponentIds (EntityT i),
    Queryable (EntityT i),
    Q.Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o
  ) =>
  (i -> m o) ->
  System m () [o]
mapM f = mapView (\v cs -> V.mapM f v cs)

mapM_ ::
  forall m i o.
  ( Monad m,
    ComponentIds (EntityT i),
    Queryable (EntityT i),
    Q.Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o
  ) =>
  (i -> m o) ->
  System m () ()
mapM_ f = mapView_ (\v cs -> snd <$> V.mapM f v cs)

mapAccum ::
  forall m i o a.
  ( Monad m,
    ComponentIds (EntityT i),
    Queryable (EntityT i),
    Q.Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o
  ) =>
  (i -> m (a, o)) ->
  System m () ([(a, o)])
mapAccum f =
  let f' i = do
        (a, o) <- lift $ f i
        tell [a]
        return o
      x = mapView (\v cs -> V.mapM f' v cs)
   in System $ \cs ->
        let (cs', cIds, g) = runSystem' x cs
         in ( cs',
              cIds,
              \_ w -> do
                ((os, w', g', _), as) <- runWriterT $ g () w
                return (zip as os, w', g', pure ())
            )

mapSingleAccum_ ::
  forall m i o a.
  ( Monad m,
    ComponentIds (EntityT i),
    Queryable (EntityT i),
    Q.Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o
  ) =>
  (i -> m (a, o)) ->
  System m () a
mapSingleAccum_ f = fst <$> mapSingleAccum f

mapSingleAccum ::
  forall m i o a.
  ( Monad m,
    ComponentIds (EntityT i),
    Queryable (EntityT i),
    Q.Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o
  ) =>
  (i -> m (a, o)) ->
  System m () (a, o)
mapSingleAccum f = fmap (fromMaybe (error "TODO")) (mapMaybeSingleAccum f)

mapMaybeSingleAccum ::
  forall m i o a.
  ( Monad m,
    ComponentIds (EntityT i),
    Queryable (EntityT i),
    Q.Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o
  ) =>
  (i -> m (a, o)) ->
  System m () (Maybe (a, o))
mapMaybeSingleAccum f =
  let f' i = do
        (a, o) <- lift $ f i
        tell [a]
        return o
      x = mapView (\v cs -> V.mapM f' v cs)
   in System $ \cs ->
        let (cs', cIds, g) = runSystem' x cs
         in ( cs',
              cIds,
              \_ w -> do
                ((os, w', g', _), as) <- runWriterT $ g () w
                case (os, as) of
                  ([o], [a]) -> return (Just (a, o), w', g', pure ())
                  _ -> return (Nothing, w', g', pure ())
            )

newtype ViewT m v a = ViewT
  {unViewT :: ReaderT (View v, Components, Map EntityID ArchetypeID) m a}
  deriving (Functor, Applicative, Monad, MonadIO)

lookupView ::
  forall m a.
  ( Monad m,
    FromEntity a,
    ComponentIds (EntityT a),
    Queryable (EntityT a),
    Lookup (Entity (EntityT a))
  ) =>
  EntityID ->
  ViewT m (EntityT a) (Maybe a)
lookupView eId = ViewT $ do
  (v, cs, es) <- ask
  return $ do
    aId <- Map.lookup eId es
    V.lookup eId v aId cs

allView ::
  forall m a.
  (Monad m, FromEntity a, ComponentIds (EntityT a), Queryable (EntityT a)) =>
  ViewT m (EntityT a) [(EntityID, a)]
allView = ViewT $ do
  (v, cs, _) <- ask
  return $ V.all @a v cs

view ::
  forall m v a.
  (Monad m, ComponentIds v, Queryable v) =>
  (ViewT m v a) ->
  System m () a
view vt = System $ \cs ->
  let (cIds, cs') = componentIds @v cs
   in ( cs',
        [cIds],
        \_ w ->
          let (v, w') = V.view @v w
           in (,w',Prelude.id,pure ()) <$> runReaderT (unViewT vt) (v, components w', entities w')
      )

viewFilter ::
  forall m v a.
  (Monad m, ComponentIds v, Queryable v) =>
  QueryFilter ->
  (ViewT m v a) ->
  System m () a
viewFilter f vt = System $ \cs ->
  let (cIds, cs') = componentIds @v cs
      (with', cs'') = filterWith f cs'
      (without', cs''') = filterWithout f cs''
      f' arch =
        F.all (\cId -> A.member cId arch) with'
          && F.all (\cId -> not (A.member cId arch)) without'
   in ( cs''',
        [cIds],
        \_ w ->
          let (v, w') = V.viewFilter @v f' w
           in (,w',Prelude.id,pure ()) <$> runReaderT (unViewT vt) (v, components w', entities w')
      )

viewFilterWith ::
  forall m v a.
  (Monad m, ComponentIds v, Queryable v) =>
  (Archetype -> Bool) ->
  (ViewT m v a) ->
  System m () a
viewFilterWith f vt = System $ \cs ->
  let (cIds, cs') = componentIds @v cs
   in ( cs',
        [cIds],
        \_ w ->
          let (v, w') = V.viewFilter @v f w
           in (,w',Prelude.id,pure ()) <$> runReaderT (unViewT vt) (v, components w', entities w')
      )

viewWith ::
  forall m i v a.
  (Monad m, ComponentIds v, Queryable v) =>
  (i -> ViewT m v a) ->
  System m i a
viewWith f = System $ \cs ->
  let (cIds, cs') = componentIds @v cs
   in ( cs',
        [cIds],
        \i w ->
          let (v, w') = V.view @v w
           in (,w',Prelude.id,pure ()) <$> runReaderT (unViewT (f i)) (v, components w', entities w')
      )

mapView ::
  forall m v a.
  (Monad m, ComponentIds v, Queryable v) =>
  (View v -> Components -> m (a, View v)) ->
  System m () a
mapView f = System $ \cs ->
  let (cIds, cs') = componentIds @v cs
   in ( cs',
        [cIds],
        \_ w ->
          let (v, w') = V.view @v w
           in (\(a, v') -> (a, w', V.unview v', pure ())) <$> f v (components w')
      )

mapView_ ::
  forall m v.
  (Monad m, ComponentIds v, Queryable v) =>
  (View v -> Components -> m (View v)) ->
  System m () ()
mapView_ f = System $ \cs ->
  let (cIds, cs') = componentIds @v cs
   in ( cs',
        [cIds],
        \_ w ->
          let (v, w') = V.view @v w
           in (\v' -> ((), w', V.unview v', pure ())) <$> f v (components w')
      )

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
