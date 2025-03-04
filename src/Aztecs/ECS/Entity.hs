{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Aztecs.ECS.Entity
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Entity (EntityID (..)) where

import Control.DeepSeq
import GHC.Generics

-- | Unique entity identifier.
--
-- @since 9.0
newtype EntityID = EntityID
  { -- | Unique integer identifier.
    --
    -- @since 9.0
    unEntityId :: Int
  }
  deriving (Eq, Ord, Show, Generic, NFData)
