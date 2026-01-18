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

import GHC.Generics

-- | Unique entity identifier.
newtype EntityID = EntityID
  { -- | Unique integer identifier.
    --
    -- @since 0.9
    unEntityId :: Int
  }
  deriving (Eq, Ord, Show, Generic)
