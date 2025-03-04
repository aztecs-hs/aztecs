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
-- @since 0.9
class (Typeable a) => Asset a where
  -- | Configuration for loading an asset.
  --
  -- @since 0.9
  type AssetConfig a

  -- | Load an asset from a file path with a configuration.
  --
  -- @since 0.9
  loadAsset :: FilePath -> AssetConfig a -> IO a
