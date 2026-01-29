{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.ECS.Component
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Component
  ( ComponentID (..),
    Component (..),
  )
where

import Aztecs.ECS.Access.Internal (Access)
import Aztecs.ECS.Component.Internal (ComponentID (..))
import Aztecs.ECS.Entity
import Aztecs.ECS.World.Storage
import Data.Typeable

-- | Component that can be stored in the `World`.
class (Monad m, Typeable a, Storage a (StorageT a)) => Component m a where
  -- | `Storage` of this component.
  type StorageT a

  type StorageT a = [a]

  -- | Lifecycle hook called when a component is inserted.
  componentOnInsert :: EntityID -> a -> Access m ()
  componentOnInsert _ _ = pure ()
  {-# INLINEABLE componentOnInsert #-}

  -- | Lifecycle hook called when a component is changed.
  componentOnChange :: EntityID -> a -> a -> Access m ()
  componentOnChange _ _ _ = pure ()
  {-# INLINEABLE componentOnChange #-}

  -- | Lifecycle hook called when a component is removed.
  componentOnRemove :: EntityID -> a -> Access m ()
  componentOnRemove _ _ = pure ()
  {-# INLINEABLE componentOnRemove #-}
