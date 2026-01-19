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

import Aztecs.ECS.Access.Internal (AccessT)
import Aztecs.ECS.Component.Internal (ComponentID (..))
import Aztecs.ECS.World.Storage
import Data.Typeable
import Data.Vector (Vector)

-- | Component that can be stored in the `World`.
class (Monad m, Typeable a, Storage a (StorageT a)) => Component m a where
  -- | `Storage` of this component.
  type StorageT a

  type StorageT a = Vector a

  -- | Lifecycle hook called when a component is inserted.
  componentOnInsert :: a -> AccessT m ()
  componentOnInsert _ = pure ()
  {-# INLINEABLE componentOnInsert #-}

  -- | Lifecycle hook called when a component is changed.
  componentOnChange :: a -> AccessT m ()
  componentOnChange _ = pure ()
  {-# INLINEABLE componentOnChange #-}

  -- | Lifecycle hook called when a component is removed.
  componentOnRemove :: a -> AccessT m ()
  componentOnRemove _ = pure ()
  {-# INLINEABLE componentOnRemove #-}
