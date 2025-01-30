{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.System where

import Data.Aztecs.Access (Access, runAccess)
import Data.Aztecs.Entity (ComponentIds, Entity, EntityT)
import Data.Aztecs.Query (IsEq, Queryable)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.View (View)
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import Data.Aztecs.World.Components (Components)

class System m a where
  task :: Task m () ()

runSystem :: forall m s. (System m s, Monad m) => World -> m World
runSystem w = do
  let (w', f) = runTask (task @m @s) w
  (_, g, access) <- f () (components w')
  (_, w'') <- runAccess access w'
  return $ g w''

newtype Task m i o = Task {runTask :: World -> (World, i -> Components -> m (o, World -> World, Access m ()))}

(<&>) :: (Monad m) => Task m i o -> Task m o a -> Task m i a
t1 <&> t2 =
  Task $ \w ->
    let (w', f) = runTask t1 w
        (w'', g) = runTask t2 w'
     in ( w'',
          \i cs -> do
            (o, f', access) <- f i cs
            (a, g', access') <- g o cs
            return (a, g' . f', access >> access')
        )

all :: forall m v. (Monad m, ComponentIds v, Queryable v) => Task m () [Entity v]
all = view @v (\v cs -> pure $ V.queryAll v cs)

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
  forall v m a.
  (Monad m, ComponentIds v, Queryable v, Functor m) =>
  (View v -> Components -> m a) ->
  Task m () a
view f = Task $ \w -> let (v, w') = V.view @v w in (w', \_ cs -> (,const w',pure ()) <$> f v cs)

mapView ::
  forall v m a.
  (Monad m, ComponentIds v, Queryable v) =>
  (View v -> Components -> m (a, View v)) ->
  Task m () a
mapView f = Task $ \w ->
  let (v, w') = V.view @v w
   in (w', \_ cs -> (\(a, v') -> (a, const $ V.unview v' w', pure ())) <$> f v cs)

-- | Queue an `Access` to alter the world after this task is complete.
queue :: (Monad m) => Access m () -> Task m () ()
queue a = Task (,\_ _ -> pure ((), id, a))

run :: (Monad m) => (i -> m o) -> Task m i o
run f =
  Task
    (,\i _ -> do
        o <- f i
        return (o, id, pure ()))
