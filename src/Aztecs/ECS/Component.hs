{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
module Aztecs.ECS.Component where

import Aztecs.ECS.World.Storage
import Data.Typeable
import Data.Vector (Vector)
import GHC.Generics

-- | Unique component identifier.
newtype ComponentID = ComponentID
  { -- | Unique integer identifier.
    unComponentId :: Int
  }
  deriving (Eq, Ord, Show, Generic)

-- | Component that can be stored in the `World`.
class (Monad m, Typeable a, Storage a (StorageT a)) => Component m a where
  -- | `Storage` of this component.
  type StorageT a

  type StorageT a = Vector a

  componentOnInsert :: a -> m ()
  componentOnInsert _ = return ()

  componentOnChange :: a -> m ()
  componentOnChange _ = return ()

  componentOnRemove :: a -> m ()
  componentOnRemove _ = return ()
