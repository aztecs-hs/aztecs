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
import Data.Aztecs.World.Archetype (Archetype, Lookup)
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Components (ComponentID, Components)
import Data.Data (Typeable)
import qualified Data.Foldable as F
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Prelude hiding (all)

class (Typeable a) => System m a where
  task :: Task m () ()

runSystem :: forall m s. (System m s, Monad m) => World -> m World
runSystem w = do
  let (cs, _, f) = runTask (task @m @s) (components w)
      w' = w {components = cs}
  (_, g, access) <- f () w'
  (_, w'') <- runAccess access w'
  return $ g w''

-- | System task.
newtype Task m i o = Task
  { runTask ::
      Components ->
      ( Components,
        [Set ComponentID],
        i -> World -> m (o, World -> World, Access m ())
      )
  }
  deriving (Functor)

instance (Monad m) => Applicative (Task m i) where
  pure a = Task (,[],\_ _ -> pure (a, Prelude.id, pure ()))
  f <*> a =
    Task $ \w ->
      let (w', cIds, f') = runTask f w
          (w'', cIds', a') = runTask a w'
       in ( w'',
            cIds <> cIds',
            \i cs -> do
              (f'', fG, access) <- f' i cs
              (a'', aG, access') <- a' i cs
              return (f'' a'', fG Prelude.. aG, access >> access')
          )

instance (Monad m) => Category (Task m) where
  id = Task (,[],\i _ -> pure (i, Prelude.id, pure ()))
  (.) t1 t2 = Task $ \w ->
    let (w', cIds, f) = runTask t2 w
        (w'', cIds', g) = runTask t1 w'
     in ( w'',
          cIds <> cIds',
          \i cs -> do
            (o, f', access) <- f i cs
            (a, g', access') <- g o cs
            return (a, g' Prelude.. f', access >> access')
        )

instance (Monad m) => Arrow (Task m) where
  arr f = Task (,[],\i _ -> pure (f i, Prelude.id, pure ()))
  first t =
    Task $ \w ->
      let (w', cIds, f) = runTask t w
       in ( w',
            cIds,
            \(i, x) cs -> do
              (o, f', access) <- f i cs
              return ((o, x), f', access)
          )

all :: forall m a. (Monad m, FromEntity a, ComponentIds (EntityT a), Queryable (EntityT a)) => Task m () [(EntityID, a)]
all = view @_ @(EntityT a) allView

allFilter ::
  forall m a.
  (Monad m, FromEntity a, ComponentIds (EntityT a), Queryable (EntityT a)) =>
  QueryFilter ->
  Task m () [(EntityID, a)]
allFilter f = viewFilter @_ @(EntityT a) f allView

single ::
  forall m a.
  ( Monad m,
    FromEntity a,
    ComponentIds (EntityT a),
    Queryable (EntityT a)
  ) =>
  Task m () a
single = fmap (fromMaybe (error "TODO")) maybeSingle

maybeSingle ::
  forall m a.
  ( Monad m,
    FromEntity a,
    ComponentIds (EntityT a),
    Queryable (EntityT a)
  ) =>
  Task m () (Maybe a)
maybeSingle = fmap (fmap snd) maybeSingleWithId

maybeSingleWithId ::
  forall m a.
  ( Monad m,
    FromEntity a,
    ComponentIds (EntityT a),
    Queryable (EntityT a)
  ) =>
  Task m () (Maybe (EntityID, a))
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
  Task m () (Maybe a)
lookup eId = view @_ @(EntityT a) (lookupView eId)

map ::
  forall m i o.
  ( Monad m,
    ComponentIds (EntityT i),
    Queryable (EntityT i),
    Q.Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o
  ) =>
  (i -> o) ->
  Task m () [o]
map f = mapView (\v cs -> pure $ V.map f v cs)

mapM ::
  forall m i o.
  ( Monad m,
    ComponentIds (EntityT i),
    Queryable (EntityT i),
    Q.Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o
  ) =>
  (i -> m o) ->
  Task m () [o]
mapM f = mapView (\v cs -> V.mapM f v cs)

mapM_ ::
  forall m i o.
  ( Monad m,
    ComponentIds (EntityT i),
    Queryable (EntityT i),
    Q.Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o
  ) =>
  (i -> m o) ->
  Task m () ()
mapM_ f = mapView_ (\v cs -> snd <$> V.mapM f v cs)

mapWith ::
  forall m i o a.
  ( Monad m,
    ComponentIds (EntityT i),
    Queryable (EntityT i),
    Q.Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o
  ) =>
  (i -> m (a, o)) ->
  Task m () ([(a, o)])
mapWith f =
  let f' i = do
        (a, o) <- lift $ f i
        tell [a]
        return o
      x = mapView (\v cs -> V.mapM f' v cs)
   in Task $ \cs ->
        let (cs', cIds, g) = runTask x cs
         in ( cs',
              cIds,
              \_ w -> do
                ((os, g', _), as) <- runWriterT $ g () w
                return (zip as os, g', pure ())
            )

mapSingleWith ::
  forall m i o a.
  ( Monad m,
    ComponentIds (EntityT i),
    Queryable (EntityT i),
    Q.Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o
  ) =>
  (i -> m (a, o)) ->
  Task m () (a, o)
mapSingleWith f = fmap (fromMaybe (error "TODO")) (mapMaybeSingleWith f)

mapMaybeSingleWith ::
  forall m i o a.
  ( Monad m,
    ComponentIds (EntityT i),
    Queryable (EntityT i),
    Q.Map (IsEq (Entity (EntityT i)) (Entity (EntityT o))) i o
  ) =>
  (i -> m (a, o)) ->
  Task m () (Maybe (a, o))
mapMaybeSingleWith f =
  let f' i = do
        (a, o) <- lift $ f i
        tell [a]
        return o
      x = mapView (\v cs -> V.mapM f' v cs)
   in Task $ \cs ->
        let (cs', cIds, g) = runTask x cs
         in ( cs',
              cIds,
              \_ w -> do
                ((os, g', _), as) <- runWriterT $ g () w
                case (os, as) of
                  ([o], [a]) -> return (Just (a, o), g', pure ())
                  _ -> return (Nothing, g', pure ())
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
  Task m () a
view vt = Task $ \cs ->
  let (cIds, cs') = componentIds @v cs
   in ( cs',
        [cIds],
        \_ w ->
          let (v, w') = V.view @v w
           in (,Prelude.id,pure ()) <$> runReaderT (unViewT vt) (v, components w', entities w')
      )

viewFilter ::
  forall m v a.
  (Monad m, ComponentIds v, Queryable v) =>
  QueryFilter ->
  (ViewT m v a) ->
  Task m () a
viewFilter f vt = Task $ \cs ->
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
           in (,Prelude.id,pure ()) <$> runReaderT (unViewT vt) (v, components w', entities w')
      )

viewFilterWith ::
  forall m v a.
  (Monad m, ComponentIds v, Queryable v) =>
  (Archetype -> Bool) ->
  (ViewT m v a) ->
  Task m () a
viewFilterWith f vt = Task $ \cs ->
  let (cIds, cs') = componentIds @v cs
   in ( cs',
        [cIds],
        \_ w ->
          let (v, w') = V.viewFilter @v f w
           in (,Prelude.id,pure ()) <$> runReaderT (unViewT vt) (v, components w', entities w')
      )

viewWith ::
  forall m i v a.
  (Monad m, ComponentIds v, Queryable v) =>
  (i -> ViewT m v a) ->
  Task m i a
viewWith f = Task $ \cs ->
  let (cIds, cs') = componentIds @v cs
   in ( cs',
        [cIds],
        \i w ->
          let (v, w') = V.view @v w
           in (,Prelude.id,pure ()) <$> runReaderT (unViewT (f i)) (v, components w', entities w')
      )

mapView ::
  forall m v a.
  (Monad m, ComponentIds v, Queryable v) =>
  (View v -> Components -> m (a, View v)) ->
  Task m () a
mapView f = Task $ \cs ->
  let (cIds, cs') = componentIds @v cs
   in ( cs',
        [cIds],
        \_ w ->
          let (v, w') = V.view @v w
           in (\(a, v') -> (a, V.unview v', pure ())) <$> f v (components w')
      )

mapView_ ::
  forall m v.
  (Monad m, ComponentIds v, Queryable v) =>
  (View v -> Components -> m (View v)) ->
  Task m () ()
mapView_ f = Task $ \cs ->
  let (cIds, cs') = componentIds @v cs
   in ( cs',
        [cIds],
        \_ w ->
          let (v, w') = V.view @v w
           in (\v' -> ((), V.unview v', pure ())) <$> f v (components w')
      )

-- | Queue an `Access` to alter the world after this task is complete.
queue :: (Monad m) => Access m () -> Task m () ()
queue a = Task (,[],\_ _ -> pure ((), Prelude.id, a))

queueWith :: (Monad m) => (i -> Access m ()) -> Task m i ()
queueWith f = Task (,[],\i _ -> pure ((), Prelude.id, f i))

run :: (Monad m) => (i -> m o) -> Task m i o
run f =
  Task
    (,[],\i _ -> do
           o <- f i
           return (o, Prelude.id, pure ()))
