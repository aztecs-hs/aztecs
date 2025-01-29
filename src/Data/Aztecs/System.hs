{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.System where

import Control.Monad ((>=>))
import Data.Aztecs.Entity (ComponentIds, Entity)
import Data.Aztecs.Query (Queryable)
import Data.Aztecs.View (View)
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import Data.Aztecs.World.Components (Components)

class System m o a where
  edit :: Edit m o

runSystem :: forall m o s. (System m o s) => World -> m o
runSystem w = let (w', f) = runEdit (edit @m @o @s) w in f (components w')

newtype Edit m a = Edit {runEdit :: World -> (World, Components -> m a)}

all :: forall m v. (Applicative m, ComponentIds v, Queryable v) => Edit m [Entity v]
all = view @v (\v cs -> pure $ V.queryAll v cs)

view :: forall v m a. (ComponentIds v, Queryable v) => (View v -> Components -> m a) -> Edit m a
view f = Edit $ \w -> let (v, w') = V.view @v w in (w', f v)

mapM :: (Monad m) => Edit m a -> (a -> m b) -> Edit m b
mapM e f = Edit $ \w -> let (w', e') = runEdit e w in (w', e' >=> f)
