{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Aztecs.Time
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.Time (Time (..)) where

import Aztecs.ECS
import Control.DeepSeq
import Data.Word
import GHC.Generics

-- | Time component.
--
-- @since 0.9
newtype Time = Time
  { -- | Elapsed time (in milliseconds).
    --
    -- @since 0.9
    elapsedMS :: Word32
  }
  deriving (Eq, Ord, Num, Show, Generic, NFData)

instance Component Time
