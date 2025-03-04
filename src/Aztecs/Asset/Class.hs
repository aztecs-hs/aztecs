{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.Asset.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.Asset.Class (Asset (..)) where

import Data.Data

-- | Loadable asset.
--
-- @since 9.0
class (Typeable a) => Asset a where
  -- | Configuration for loading an asset.
  --
  -- @since 9.0
  type AssetConfig a

  -- | Load an asset from a file path with a configuration.
  --
  -- @since 9.0
  loadAsset :: FilePath -> AssetConfig a -> IO a
