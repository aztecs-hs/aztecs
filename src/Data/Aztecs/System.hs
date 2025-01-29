{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.System where

import Data.Aztecs.Entity (ComponentIds, Entity)
import Data.Aztecs.Query (Queryable)
import Data.Aztecs.View (View)
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import Data.Aztecs.World.Components (Components)

class System m a where
  edit :: Edit m ()

runSystem :: forall m s. (System m s, Monad m) => World -> m World
runSystem w = do
  let (w', f) = runEdit (edit @m @s) w
  (_, g) <- f (components w')
  return $ g w

newtype Edit m a = Edit {runEdit :: World -> (World, Components -> m (a, World -> World))}

all :: forall m v. (Applicative m, ComponentIds v, Queryable v) => Edit m [Entity v]
all = view @v (\v cs -> pure $ V.queryAll v cs)

view :: forall v m a. (ComponentIds v, Queryable v, Functor m) => (View v -> Components -> m a) -> Edit m a
view f = Edit $ \w -> let (v, w') = V.view @v w in (w', fmap (,const w) . f v)

mapView :: forall v m a. (ComponentIds v, Queryable v, Functor m) => (View v -> Components -> m (a, View v)) -> Edit m a
mapView f = Edit $ \w -> let (v, w') = V.view @v w in (w', fmap (\(a, v') -> (a, const $ V.unview v' w')) . f v)

mapM :: (Monad m) => Edit m a -> (a -> m b) -> Edit m b
mapM e f = Edit $ \w ->
  let (w', e') = runEdit e w
   in ( w',
        \cs -> do
          (a, g) <- e' cs
          b <- f a
          return (b, g)
      )
