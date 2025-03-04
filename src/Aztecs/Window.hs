{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Aztecs.Window
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.Window (Window (..)) where

import Aztecs.ECS
import Control.DeepSeq
import GHC.Generics

-- | Window component.
--
-- @since 0.9
newtype Window = Window
  { -- | Window title.
    --
    -- @since 0.9
    windowTitle :: String
  }
  deriving (Show, Generic, NFData)

-- | @since 0.9
instance Component Window
