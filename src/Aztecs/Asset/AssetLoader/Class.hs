{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      : Aztecs.Asset.AssetLoader.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.Asset.AssetLoader.Class (MonadAssetLoader (..)) where

import Aztecs.Asset.AssetServer
import Aztecs.Asset.Class

-- | Monadic interface for loading assets.
--
-- @since 9.0
class MonadAssetLoader a m | m -> a where
  -- | Load an asset from a file path with a configuration.
  --
  -- @since 9.0
  asset :: FilePath -> AssetConfig a -> m (Handle a)
