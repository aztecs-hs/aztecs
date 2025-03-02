{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Aztecs.ECS.Query.Dynamic.Class (ArrowDynamicQuery (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader)
import Control.Arrow

class (ArrowDynamicQueryReader arr) => ArrowDynamicQuery m arr | arr -> m where
  adjustDyn :: (Component a) => (i -> a -> a) -> ComponentID -> arr i a

  adjustDyn_ :: (Component a) => (i -> a -> a) -> ComponentID -> arr i ()
  adjustDyn_ f cid = adjustDyn @m f cid >>> arr (const ())

  adjustDynM :: (Component a) => (i -> a -> m a) -> ComponentID -> arr i a

  setDyn :: (Component a) => ComponentID -> arr a a
