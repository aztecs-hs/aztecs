{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.Class (ECS (..)) where

import Aztecs.ECS.HSet
import Data.Kind

class ECS m where
  type Entity m :: Type
  type Components m :: [Type]
  type Bundle m :: Type
  type Task m :: Type -> Type

  spawn :: Bundle m -> m (Entity m)

  insert :: Entity m -> Bundle m -> m ()

  remove :: Entity m -> m ()

  task :: (Task m) a -> m a
