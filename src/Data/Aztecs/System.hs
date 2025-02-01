{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.System where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Data.Aztecs.Access (Access, runAccess)
import Data.Aztecs.Entity (ComponentIds (componentIds), Entity, EntityT)
import Data.Aztecs.Query (IsEq, Queryable)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.View (View (..))
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import Data.Aztecs.World.Components (ComponentID, Components)
import Data.Data (Typeable)
import Data.Set (Set)

class (Typeable a) => System m a where
  task :: Task m () ()

runSystem :: forall m s. (System m s, Monad m) => World -> m World
runSystem w = do
  let (w', _, f) = runTask (task @m @s) w
  (_, g, access) <- f () (components w')
  (_, w'') <- runAccess access w'
  return $ g w''

-- | System task.
newtype Task m i o = Task
  { runTask ::
      World ->
      ( World,
        [Set ComponentID],
        i -> Components -> m (o, World -> World, Access m ())
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

all :: forall m v. (Monad m, ComponentIds v, Queryable v) => Task m () [Entity v]
all = view @_ @v (\v cs -> pure $ V.queryAll v cs)

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

view ::
  forall m v a.
  (Monad m, ComponentIds v, Queryable v) =>
  (View v -> Components -> m a) ->
  Task m () a
view f = Task $ \w ->
  let (cIds, cs') = componentIds @v (components w)
      (v, w') = V.view @v w {components = cs'}
   in (w', [cIds], \_ cs -> (,Prelude.id,pure ()) <$> f v cs)

mapView ::
  forall m v a.
  (Monad m, ComponentIds v, Queryable v) =>
  (View v -> Components -> m (a, View v)) ->
  Task m () a
mapView f = Task $ \w ->
  let (cIds, cs') = componentIds @v (components w)
      (v, w') = V.view @v w {components = cs'}
   in (w', [cIds], \_ cs -> (\(a, v') -> (a, V.unview v', pure ())) <$> f v cs)

-- | Queue an `Access` to alter the world after this task is complete.
queue :: (Monad m) => Access m () -> Task m () ()
queue a = Task (,[],\_ _ -> pure ((), Prelude.id, a))

run :: (Monad m) => (i -> m o) -> Task m i o
run f =
  Task
    (,[],\i _ -> do
           o <- f i
           return (o, Prelude.id, pure ()))
