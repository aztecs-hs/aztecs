{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Aztecs.ECS.Component.Internal
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Component.Internal (ComponentID (..)) where

import GHC.Generics

-- | Unique component identifier.
newtype ComponentID = ComponentID
  { -- | Unique integer identifier.
    unComponentId :: Int
  }
  deriving (Eq, Ord, Show, Generic)
