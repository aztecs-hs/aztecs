{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.ECS.Component
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Component where

import Aztecs.ECS.World.Storage
import Data.Typeable
import Data.Vector (Vector)
import GHC.Generics

-- | Unique component identifier.
newtype ComponentID = ComponentID
  { -- | Unique integer identifier.
    --
    -- @since 0.9
    unComponentId :: Int
  }
  deriving (Eq, Ord, Show, Generic)

-- | Component that can be stored in the `World`.
class (Typeable a, Storage a (StorageT a)) => Component a where
  -- | `Storage` of this component.
  --
  -- @since 0.9
  type StorageT a

  type StorageT a = Vector a
